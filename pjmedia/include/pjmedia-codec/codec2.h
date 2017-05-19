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
#ifndef __PJMEDIA_CODEC_CODEC2_H__
#define __PJMEDIA_CODEC_CODEC2_H__

/**
 * @file pjmedia-codec/codec2.h
 * @brief Codec 2 codec.
 */

#include <pjmedia-codec/types.h>

/**
 * @defgroup PJMED_CODEC2 Codec 2 Codec
 * @ingroup PJMEDIA_CODEC_CODECS
 * @brief Codec 2 codec wrapper
 * @{
 *
 * This section describes functions to initialize and register Codec 2 codec
 * factory to the codec manager. After the codec factory has been registered,
 * application can use @ref PJMEDIA_CODEC API to manipulate the codec.
 *
 * The Codec 2 codec supports multiple bit rates with sampling rate of 8000Hz.
 *
 *
 * \section codec_setting Codec Settings
 *
 * \subsection general_setting General Settings
 *
 * General codec settings for this codec such as VAD and PLC can be
 * manipulated through the <tt>setting</tt> field in #pjmedia_codec_param.
 * Please see the documentation of #pjmedia_codec_param for more info.
 *
 * \subsection specific_setting Codec Specific Settings
 *
 * Currently none.
 */

PJ_BEGIN_DECL

typedef struct pjmedia_codec_codec2_setting
{
    int         mode;       /**< Codec 2 mode, or use -1 for default
                                 (@see PJMEDIA_CODEC_CODEC2_DEFAULT_MODE)  */
} pjmedia_codec_codec2_setting;


/**
 * Initialize and register Codec 2 codec factory to pjmedia endpoint.
 *
 * @param endpt		The pjmedia endpoint.
 *
 * @return		PJ_SUCCESS on success.
 */
PJ_DECL(pj_status_t) pjmedia_codec_codec2_init(pjmedia_endpt *endpt);

/**
 * Unregister Codec 2 codec factory from pjmedia endpoint and deinitialize
 * the Codec 2 codec library.
 *
 * @return	    PJ_SUCCESS on success.
 */
PJ_DECL(pj_status_t) pjmedia_codec_codec2_deinit(void);

/**
 * Change the configuration setting of the Codec 2 codec for the specified
 * mode.
 *
 * @param mode		 Mode to use. Valid values are:
 *			 0: 3200 bit/s
 *			 1: 2400 bit/s
 *			 2: 1600 bit/s
 *			 3: 1400 bit/s
 *			 4: 1300 bit/s
 *			 5: 1200 bit/s
 *			 6:  700 bit/s
 *			 7:  700 bit/s version b
 *			 8:  700 bit/s version c
 *			-1: identical to 0
 * @param opt		The setting to be applied for the specified mode.
 *
 * @return		PJ_SUCCESS on success.
 */
PJ_DECL(pj_status_t) pjmedia_codec_codec2_set_config(
				unsigned mode,
				const pjmedia_codec_codec2_setting *opt);


PJ_END_DECL


/**
 * @}
 */

#endif	/* __PJMEDIA_CODEC_CODEC2_H__ */
