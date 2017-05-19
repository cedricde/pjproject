/* $Id$ */
/*
 * Copyright (C) 2017 Teluu Inc. (http://www.teluu.com)
 * Contributed by Cedric Delmas
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include <pjmedia-codec/codec2.h>
#include <pjmedia/codec.h>
#include <pjmedia/endpoint.h>
#include <pjmedia/errno.h>
#include <pjmedia/plc.h>
#include <pjmedia/silencedet.h>
#include <pj/assert.h>
#include <pj/log.h>
#include <pj/pool.h>
#include <pj/string.h>
#include <pj/os.h>

/*
 * Only build this file if PJMEDIA_HAS_CODEC2_CODEC != 0
 */
#if defined(PJMEDIA_HAS_CODEC2_CODEC) && PJMEDIA_HAS_CODEC2_CODEC != 0

#include <codec2/codec2.h>

#define THIS_FILE "codec2.c"


/* Current Codec 2 mode */
#define CODEC2_MODE                 PJMEDIA_CODEC_CODEC2_DEFAULT_MODE


/* Bitrates for each mode */
#define F_CODEC2_BITRATE(m) \
    (m == 0 ? 3200 : \
     m == 1 ? 2400 : \
     m == 2 ? 1600 : \
     m == 3 ? 1400 : \
     m == 4 ? 1300 : \
     m == 5 ? 1200 : \
               700)

/* Samples per frame for each mode */
#define F_CODEC2_SAMPLES_PER_FRAME(m) \
    (m == 0 || m == 1 ? 160 : 320)

/* Bits per frame for each mode */
#define F_CODEC2_BITS_PER_FRAME(m) \
    ((m == 0 || m == 2) ? 64 : \
     (m == 1 || m == 5) ? 48 : \
      m == 3            ? 56 : \
      m == 4            ? 52 : \
                          28)

#define CODEC2_BITRATE              F_CODEC2_BITRATE(CODEC2_MODE)
#define CODEC2_SAMPLES_PER_FRAME    F_CODEC2_SAMPLES_PER_FRAME(CODEC2_MODE)
#define CODEC2_BITS_PER_FRAME       F_CODEC2_BITS_PER_FRAME(CODEC2_MODE)
#define CODEC2_BYTES_PER_FRAME      ((CODEC2_BITS_PER_FRAME + 7) / 8)
#define CODEC2_FRAME_LENGTH         (CODEC2_SAMPLES_PER_FRAME / 8)


#define PLC_DISABLED	0


/* Prototypes for Codec 2 factory */
static pj_status_t codec2_test_alloc(pjmedia_codec_factory *factory,
				     const pjmedia_codec_info *id);
static pj_status_t codec2_default_attr(pjmedia_codec_factory *factory,
				       const pjmedia_codec_info *id,
				       pjmedia_codec_param *attr);
static pj_status_t codec2_enum_codecs(pjmedia_codec_factory *factory,
				      unsigned *count,
				      pjmedia_codec_info codecs[]);
static pj_status_t codec2_alloc_codec(pjmedia_codec_factory *factory,
				      const pjmedia_codec_info *id,
				      pjmedia_codec **p_codec);
static pj_status_t codec2_dealloc_codec(pjmedia_codec_factory *factory,
					pjmedia_codec *codec);

/* Prototypes for Codec 2 implementation. */
static pj_status_t  codec2_codec_init(pjmedia_codec *codec,
				      pj_pool_t *pool);
static pj_status_t  codec2_codec_open(pjmedia_codec *codec,
				      pjmedia_codec_param *attr);
static pj_status_t  codec2_codec_close(pjmedia_codec *codec);
static pj_status_t  codec2_codec_modify(pjmedia_codec *codec,
					const pjmedia_codec_param *attr);
static pj_status_t  codec2_codec_parse(pjmedia_codec *codec,
				       void *pkt,
				       pj_size_t pkt_size,
				       const pj_timestamp *ts,
				       unsigned *frame_cnt,
				       pjmedia_frame frames[]);
static pj_status_t  codec2_codec_encode(pjmedia_codec *codec,
					const struct pjmedia_frame *input,
					unsigned output_buf_len,
					struct pjmedia_frame *output);
static pj_status_t  codec2_codec_decode(pjmedia_codec *codec,
					const struct pjmedia_frame *input,
					unsigned output_buf_len,
					struct pjmedia_frame *output);
#if !PLC_DISABLED
static pj_status_t  codec2_codec_recover(pjmedia_codec *codec,
					 unsigned output_buf_len,
					 struct pjmedia_frame *output);
#endif

/* Definition for Codec 2 codec operations. */
static pjmedia_codec_op codec2_op =
{
    &codec2_codec_init,
    &codec2_codec_open,
    &codec2_codec_close,
    &codec2_codec_modify,
    &codec2_codec_parse,
    &codec2_codec_encode,
    &codec2_codec_decode,
#if !PLC_DISABLED
    &codec2_codec_recover
#else
    NULL
#endif
};

/* Definition for Codec 2 codec factory operations. */
static pjmedia_codec_factory_op codec2_factory_op =
{
    &codec2_test_alloc,
    &codec2_default_attr,
    &codec2_enum_codecs,
    &codec2_alloc_codec,
    &codec2_dealloc_codec,
    &pjmedia_codec_codec2_deinit
};

/* Codec 2 factory */
static struct codec2_codec_factory
{
    pjmedia_codec_factory   base;
    pjmedia_endpt          *endpt;
    pj_pool_t              *pool;
} codec2_codec_factory;


/* Codec 2 codec private data. */
struct codec2_data
{
    pj_pool_t              *pool;
    struct CODEC2          *encoder;
    struct CODEC2          *decoder;
    pj_bool_t               plc_enabled;
    pj_bool_t               vad_enabled;
#if !PLC_DISABLED
    pjmedia_plc            *plc;
#endif
    pjmedia_silence_det    *vad;
    pj_timestamp            last_tx;
};



/*
 * Initialize and register Codec 2 codec factory to pjmedia endpoint.
 */
PJ_DEF(pj_status_t) pjmedia_codec_codec2_init(pjmedia_endpt *endpt)
{
    pjmedia_codec_mgr *codec_mgr;
    pj_status_t status;

    PJ_ASSERT_RETURN(endpt, PJ_EINVAL);

    if (codec2_codec_factory.pool != NULL)
        return PJ_SUCCESS;

    /* Create Codec 2 codec factory. */
    pj_bzero(&codec2_codec_factory, sizeof(codec2_codec_factory));
    codec2_codec_factory.base.op = &codec2_factory_op;
    codec2_codec_factory.base.factory_data = NULL;
    codec2_codec_factory.endpt = endpt;

    codec2_codec_factory.pool = pjmedia_endpt_create_pool(endpt, "codec2",
                                                          1024, 1024);
    if (!codec2_codec_factory.pool) {
        PJ_LOG(2, (THIS_FILE, "Unable to create memory pool for Codec 2"));
        return PJ_ENOMEM;
    }

    /* Get the codec manager. */
    codec_mgr = pjmedia_endpt_get_codec_mgr(endpt);
    if (!codec_mgr) {
        PJ_LOG(2, (THIS_FILE, "Unable to get the codec manager"));
        status = PJ_EINVALIDOP;
        goto on_error;
    }

    /* Register codec factory to endpoint. */
    status = pjmedia_codec_mgr_register_factory(codec_mgr,
                                                &codec2_codec_factory.base);
    if (status != PJ_SUCCESS) {
        PJ_LOG(2, (THIS_FILE, "Unable to register the codec factory"));
        goto on_error;
    }

    return PJ_SUCCESS;

on_error:
    pj_pool_release(codec2_codec_factory.pool);
    codec2_codec_factory.pool = NULL;
    return status;
}



/*
 * Unregister Codec 2 codec factory from pjmedia endpoint and deinitialize
 * the Codec 2 codec library.
 */
PJ_DEF(pj_status_t) pjmedia_codec_codec2_deinit(void)
{
    pjmedia_codec_mgr *codec_mgr;
    pj_status_t status;

    if (codec2_codec_factory.pool == NULL)
        return PJ_SUCCESS;

    /* Get the codec manager. */
    codec_mgr = pjmedia_endpt_get_codec_mgr(codec2_codec_factory.endpt);
    if (!codec_mgr) {
        PJ_LOG(2, (THIS_FILE, "Unable to get the codec manager"));
        pj_pool_release(codec2_codec_factory.pool);
        codec2_codec_factory.pool = NULL;
        return PJ_EINVALIDOP;
    }

    /* Unregister Codec 2 codec factory. */
    status = pjmedia_codec_mgr_unregister_factory(codec_mgr,
                                                  &codec2_codec_factory.base);
    if (status != PJ_SUCCESS)
        PJ_LOG(2, (THIS_FILE, "Unable to unregister the codec factory"));

    /* Destroy pool. */
    pj_pool_release(codec2_codec_factory.pool);
    codec2_codec_factory.pool = NULL;

    return status;
}

/*
 * Check if factory can allocate the specified codec.
 */
static pj_status_t codec2_test_alloc(pjmedia_codec_factory *factory,
                                     const pjmedia_codec_info *info)
{
    static const pj_str_t codec2_tag = { "CODEC2", 6 };

    PJ_UNUSED_ARG(factory);
    PJ_ASSERT_RETURN(factory==&codec2_codec_factory.base, PJ_EINVAL);

    /* Type must be audio */
    if (info->type != PJMEDIA_TYPE_AUDIO)
        return PJMEDIA_CODEC_EUNSUP;

    /* Check encoding name */
    if (pj_stricmp(&info->encoding_name, &codec2_tag) != 0)
        return PJMEDIA_CODEC_EUNSUP;

    /* Channel count must be one */
    if (info->channel_cnt != 1)
        return PJMEDIA_CODEC_EUNSUP;

    /* Check clock-rate */
    if (info->clock_rate != 8000)
        return PJMEDIA_CODEC_EUNSUP;

    return PJ_SUCCESS;
}

/*
 * Generate default attribute.
 */
static pj_status_t codec2_default_attr(pjmedia_codec_factory *factory,
                                       const pjmedia_codec_info *ci,
                                       pjmedia_codec_param *attr)
{
    PJ_UNUSED_ARG(factory);

    pj_bzero(attr, sizeof(pjmedia_codec_param));
    attr->info.clock_rate = 8000;
    attr->info.channel_cnt = 1;
    attr->info.avg_bps = CODEC2_BITRATE;
    attr->info.max_bps = CODEC2_BITRATE;
    attr->info.frm_ptime = CODEC2_FRAME_LENGTH;
    attr->info.pcm_bits_per_sample = 16;
    attr->info.pt = (pj_uint8_t)ci->pt;
    attr->setting.frm_per_pkt = 1;
    attr->setting.vad = 1;
#if !PLC_DISABLED
    attr->setting.plc = 1;
#endif

    /* Default all other flag bits disabled. */

    return PJ_SUCCESS;
}

/*
 * Enum codecs supported by this factory.
 */
static pj_status_t codec2_enum_codecs(pjmedia_codec_factory *factory,
                                      unsigned *count,
                                      pjmedia_codec_info codecs[])
{
    PJ_UNUSED_ARG(factory);
    PJ_ASSERT_RETURN(codecs && *count > 0, PJ_EINVAL);

    pj_bzero(&codecs[0], sizeof(pjmedia_codec_info));
    codecs[0].type = PJMEDIA_TYPE_AUDIO;
    codecs[0].pt = PJMEDIA_RTP_PT_CODEC2;
    codecs[0].encoding_name = pj_str("codec2");
    codecs[0].clock_rate = 8000;
    codecs[0].channel_cnt = 1;

    *count = 1;

    return PJ_SUCCESS;
}

/*
 * Allocate a new Codec 2 codec instance.
 */
static pj_status_t codec2_alloc_codec(pjmedia_codec_factory *factory,
                                      const pjmedia_codec_info *ci,
                                      pjmedia_codec **p_codec)
{
    pj_pool_t *pool;
    pjmedia_codec *codec;
    struct codec2_data *codec2_data;
    pj_status_t status;

    PJ_ASSERT_RETURN(factory && ci && p_codec, PJ_EINVAL);
    PJ_ASSERT_RETURN(factory == &codec2_codec_factory.base, PJ_EINVAL);

    /* Create pool for codec instance */
    pool = pjmedia_endpt_create_pool(((struct codec2_codec_factory*)factory)->endpt,
                                     "codec2", 512, 512);
    if (!pool) {
        PJ_LOG(2, (THIS_FILE, "Unable to create pool for Codec 2 instance"));
        return PJ_ENOMEM;
    }

    /* Allocate codec */
    codec = PJ_POOL_ALLOC_T(pool, pjmedia_codec);
    PJ_ASSERT_RETURN(codec != NULL, PJ_ENOMEM);

    codec->op = &codec2_op;
    codec->factory = factory;

    codec2_data = PJ_POOL_ZALLOC_T(pool, struct codec2_data);
    PJ_ASSERT_RETURN(codec2_data != NULL, PJ_ENOMEM);

    codec2_data->pool = pool;

    codec->codec_data = codec2_data;

#if !PLC_DISABLED
    /* Create PLC */
    status = pjmedia_plc_create(pool, 8000, CODEC2_SAMPLES_PER_FRAME,
                                0, &codec2_data->plc);
    if (status != PJ_SUCCESS) {
        PJ_LOG(2, (THIS_FILE, "Unable to create PLC"));
        return status;
    }
#endif

    /* Create silence detector */
    status = pjmedia_silence_det_create(pool, 8000, CODEC2_SAMPLES_PER_FRAME,
                                        &codec2_data->vad);
    if (status != PJ_SUCCESS) {
        PJ_LOG(2, (THIS_FILE, "Unable to create silence detector"));
        return status;
    }

    *p_codec = codec;
    return PJ_SUCCESS;
}

/*
 * Free codec.
 */
static pj_status_t codec2_dealloc_codec(pjmedia_codec_factory *factory,
                                        pjmedia_codec *codec)
{
    PJ_ASSERT_RETURN(factory && codec, PJ_EINVAL);
    PJ_ASSERT_RETURN(factory == &codec2_codec_factory.base, PJ_EINVAL);

    /* Close codec, if it's not closed. */
    codec2_codec_close(codec);

    if (codec->codec_data) {
        pj_pool_release(((struct codec2_data*)codec->codec_data)->pool);
    }

    return PJ_SUCCESS;
}

/*
 * Init codec.
 */
static pj_status_t codec2_codec_init(pjmedia_codec *codec,
                                     pj_pool_t *pool)
{
    PJ_UNUSED_ARG(codec);
    PJ_UNUSED_ARG(pool);
    return PJ_SUCCESS;
}

/*
 * Open codec.
 */
static pj_status_t codec2_codec_open(pjmedia_codec *codec,
                                     pjmedia_codec_param *attr)
{
    struct codec2_data *codec2_data;

    PJ_ASSERT_RETURN(codec && codec->codec_data && attr, PJ_EINVAL);

    codec2_data = (struct codec2_data*) codec->codec_data;

    pj_assert(codec2_data->encoder == NULL && codec2_data->decoder == NULL);

    codec2_data->encoder = codec2_create(CODEC2_MODE);
    if (!codec2_data->encoder)
        return PJMEDIA_CODEC_EFAILED;

    codec2_data->decoder = codec2_create(CODEC2_MODE);
    if (!codec2_data->decoder)
	return PJMEDIA_CODEC_EFAILED;

    codec2_data->vad_enabled = (attr->setting.vad != 0);
    codec2_data->plc_enabled = (attr->setting.plc != 0);

    return PJ_SUCCESS;
}

/*
 * Close codec.
 */
static pj_status_t codec2_codec_close(pjmedia_codec *codec)
{
    struct codec2_data *codec2_data;

    PJ_ASSERT_RETURN(codec && codec->codec_data, PJ_EINVAL);

    codec2_data = (struct codec2_data*) codec->codec_data;

    if (codec2_data->encoder) {
	codec2_destroy(codec2_data->encoder);
	codec2_data->encoder = NULL;
    }
    if (codec2_data->decoder) {
	codec2_destroy(codec2_data->decoder);
	codec2_data->decoder = NULL;
    }

    return PJ_SUCCESS;
}


/*
 * Modify codec settings.
 */
static pj_status_t codec2_codec_modify(pjmedia_codec *codec,
                                       const pjmedia_codec_param *attr)
{
    struct codec2_data *codec2_data;

    PJ_ASSERT_RETURN(codec && codec->codec_data && attr, PJ_EINVAL);

    codec2_data = (struct codec2_data*)codec->codec_data;

    pj_assert(codec2_data->encoder != NULL && codec2_data->decoder != NULL);

    codec2_data->vad_enabled = (attr->setting.vad != 0);
    codec2_data->plc_enabled = (attr->setting.plc != 0);

    return PJ_SUCCESS;
}


/*
 * Get frames in the packet.
 */
static pj_status_t codec2_codec_parse(pjmedia_codec *codec,
                                      void *pkt,
                                      pj_size_t pkt_size,
                                      const pj_timestamp *ts,
                                      unsigned *frame_cnt,
                                      pjmedia_frame frames[])
{
    unsigned count = 0;

    PJ_UNUSED_ARG(codec);

    PJ_ASSERT_RETURN(frame_cnt, PJ_EINVAL);

    while (pkt_size >= CODEC2_BYTES_PER_FRAME && count < *frame_cnt) {
	frames[count].type = PJMEDIA_FRAME_TYPE_AUDIO;
	frames[count].buf = pkt;
	frames[count].size = CODEC2_BYTES_PER_FRAME;
	frames[count].timestamp.u64 = ts->u64 + count * CODEC2_SAMPLES_PER_FRAME;

	pkt = ((char*)pkt) + CODEC2_BYTES_PER_FRAME;
	pkt_size -= CODEC2_BYTES_PER_FRAME;

	++count;
    }

    *frame_cnt = count;
    return PJ_SUCCESS;
}

/*
 * Encode frame.
 */
static pj_status_t codec2_codec_encode(pjmedia_codec *codec,
                                       const struct pjmedia_frame *input,
                                       unsigned output_buf_len,
                                       struct pjmedia_frame *output)
{
    struct codec2_data *codec2_data = (struct codec2_data*) codec->codec_data;
    pj_int16_t *pcm_in;
    pj_size_t in_size;

    pj_assert(codec2_data && input && output);

    pcm_in = (pj_int16_t*)input->buf;
    in_size = input->size;

    PJ_ASSERT_RETURN(in_size % (CODEC2_SAMPLES_PER_FRAME*2) == 0,
                     PJMEDIA_CODEC_EPCMFRMINLEN);
    PJ_ASSERT_RETURN(output_buf_len >= CODEC2_BYTES_PER_FRAME
                                       * in_size/(CODEC2_SAMPLES_PER_FRAME*2),
                     PJMEDIA_CODEC_EFRMTOOSHORT);

    /* Detect silence */
    if (codec2_data->vad_enabled) {
        pj_bool_t is_silence;
        pj_int32_t silence_duration;

        silence_duration = pj_timestamp_diff32(&codec2_data->last_tx,
                                               &input->timestamp);

        is_silence = pjmedia_silence_det_detect(codec2_data->vad,
                                                (const pj_int16_t*) input->buf,
                                                (input->size >> 1),
                                                NULL);
        if (is_silence &&
            (PJMEDIA_CODEC_MAX_SILENCE_PERIOD == -1 ||
                silence_duration < PJMEDIA_CODEC_MAX_SILENCE_PERIOD*8000/1000))
        {
            output->type = PJMEDIA_FRAME_TYPE_NONE;
            output->buf = NULL;
            output->size = 0;
            output->timestamp = input->timestamp;
            return PJ_SUCCESS;
        } else {
            codec2_data->last_tx = input->timestamp;
        }
    }

    /* Encode */
    output->size = 0;
    while (in_size >= (CODEC2_SAMPLES_PER_FRAME*2)) {
        codec2_encode(codec2_data->encoder,
                      (unsigned char*)output->buf + output->size,
                      (short *)pcm_in);
        pcm_in += CODEC2_SAMPLES_PER_FRAME;
        output->size += CODEC2_BYTES_PER_FRAME;
        in_size -= (CODEC2_SAMPLES_PER_FRAME*2);
    }

    output->type = PJMEDIA_FRAME_TYPE_AUDIO;
    output->timestamp = input->timestamp;

    return PJ_SUCCESS;
}

/*
 * Decode frame.
 */
static pj_status_t codec2_codec_decode(pjmedia_codec *codec,
                                       const struct pjmedia_frame *input,
                                       unsigned output_buf_len,
                                       struct pjmedia_frame *output)
{
    struct codec2_data *codec2_data = (struct codec2_data*)codec->codec_data;

    pj_assert(codec2_data != NULL);
    PJ_ASSERT_RETURN(input && output, PJ_EINVAL);

    if (output_buf_len < (CODEC2_SAMPLES_PER_FRAME*2))
        return PJMEDIA_CODEC_EPCMTOOSHORT;

    if (input->size < CODEC2_BYTES_PER_FRAME)
        return PJMEDIA_CODEC_EFRMTOOSHORT;

    codec2_decode(codec2_data->decoder,
                  (short *)output->buf,
                  (const unsigned char *)input->buf);

    output->size = CODEC2_SAMPLES_PER_FRAME*2;
    output->type = PJMEDIA_FRAME_TYPE_AUDIO;
    output->timestamp = input->timestamp;

#if !PLC_DISABLED
    if (codec2_data->plc_enabled)
	pjmedia_plc_save(codec2_data->plc, (pj_int16_t*)output->buf);
#endif

    return PJ_SUCCESS;
}


#if !PLC_DISABLED
/*
 * Recover lost frame.
 */
static pj_status_t codec2_codec_recover(pjmedia_codec *codec,
                                        unsigned output_buf_len,
                                        struct pjmedia_frame *output)
{
    struct codec2_data *codec2_data = (struct codec2_data*)codec->codec_data;

    PJ_ASSERT_RETURN(codec2_data->plc_enabled, PJ_EINVALIDOP);

    PJ_ASSERT_RETURN(output_buf_len >= (CODEC2_SAMPLES_PER_FRAME*2),
                     PJMEDIA_CODEC_EPCMTOOSHORT);

    pjmedia_plc_generate(codec2_data->plc, (pj_int16_t*)output->buf);
    output->size = CODEC2_SAMPLES_PER_FRAME * 2;

    return PJ_SUCCESS;
}
#endif


#endif	/* PJMEDIA_HAS_CODEC2_CODEC */
