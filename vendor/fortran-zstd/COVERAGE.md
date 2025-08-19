# Coverage

| C function                                 | Fortran interface                 | Notes                      |
|--------------------------------------------|-----------------------------------|----------------------------|
| `ZSTD_CCtxParams_getParameter`             |                                   |                            |
| `ZSTD_CCtxParams_init`                     |                                   |                            |
| `ZSTD_CCtxParams_init_advanced`            |                                   |                            |
| `ZSTD_CCtxParams_registerSequenceProducer` |                                   |                            |
| `ZSTD_CCtxParams_reset`                    |                                   |                            |
| `ZSTD_CCtxParams_setParameter`             |                                   |                            |
| `ZSTD_CCtx_getParameter`                   |                                   |                            |
| `ZSTD_CCtx_loadDictionary`                 |                                   |                            |
| `ZSTD_CCtx_loadDictionary_advanced`        |                                   |                            |
| `ZSTD_CCtx_loadDictionary_byReference`     |                                   |                            |
| `ZSTD_CCtx_refCDict`                       |                                   |                            |
| `ZSTD_CCtx_refPrefix`                      |                                   |                            |
| `ZSTD_CCtx_refPrefix_advanced`             |                                   |                            |
| `ZSTD_CCtx_refThreadPool`                  |                                   |                            |
| `ZSTD_CCtx_reset`                          | `zstd_c_ctx_reset`                |                            |
| `ZSTD_CCtx_setCParams`                     |                                   |                            |
| `ZSTD_CCtx_setFParams`                     |                                   |                            |
| `ZSTD_CCtx_setParameter`                   | `zstd_c_ctx_set_parameter`        |                            |
| `ZSTD_CCtx_setParametersUsingCCtxParams`   |                                   |                            |
| `ZSTD_CCtx_setParams`                      |                                   |                            |
| `ZSTD_CCtx_setPledgedSrcSize`              | `zstd_c_ctx_set_pledged_src_size` |                            |
| `ZSTD_CStreamInSize`                       | `zstd_c_stream_in_size`           |                            |
| `ZSTD_CStreamOutSize`                      | `zstd_c_stream_out_size`          |                            |
| `ZSTD_DCtx_getParameter`                   |                                   |                            |
| `ZSTD_DCtx_loadDictionary`                 |                                   |                            |
| `ZSTD_DCtx_loadDictionary_advanced`        |                                   |                            |
| `ZSTD_DCtx_loadDictionary_byReference`     |                                   |                            |
| `ZSTD_DCtx_refDDict`                       |                                   |                            |
| `ZSTD_DCtx_refPrefix`                      |                                   |                            |
| `ZSTD_DCtx_refPrefix_advanced`             |                                   |                            |
| `ZSTD_DCtx_reset`                          | `zstd_d_ctx_reset`                |                            |
| `ZSTD_DCtx_setFormat`                      |                                   | deprecated                 |
| `ZSTD_DCtx_setMaxWindowSize`               |                                   |                            |
| `ZSTD_DCtx_setParameter`                   | `zstd_d_ctx_set_parameter`        |                            |
| `ZSTD_DStreamInSize`                       | `zstd_d_stream_in_size`           |                            |
| `ZSTD_DStreamOutSize`                      | `zstd_d_stream_out_size`          |                            |
| `ZSTD_adjustCParams`                       |                                   |                            |
| `ZSTD_cParam_getBounds`                    | `zstd_c_param_get_bounds`         |                            |
| `ZSTD_checkCParams`                        |                                   |                            |
| `ZSTD_compress`                            | `zstd_compress`                   |                            |
| `ZSTD_compress2`                           | `zstd_compress2`                  |                            |
| `ZSTD_compressBegin`                       |                                   | deprecated                 |
| `ZSTD_compressBegin_advanced`              |                                   | deprecated                 |
| `ZSTD_compressBegin_usingCDict`            |                                   | deprecated                 |
| `ZSTD_compressBegin_usingCDict_advanced`   |                                   | deprecated                 |
| `ZSTD_compressBegin_usingDict`             |                                   | deprecated                 |
| `ZSTD_compressBlock`                       |                                   | deprecated                 |
| `ZSTD_compressBound`                       | `zstd_compress_bound`             |                            |
| `ZSTD_compressCCtx`                        | `zstd_compress_c_ctx`             |                            |
| `ZSTD_compressContinue`                    |                                   | deprecated                 |
| `ZSTD_compressEnd`                         |                                   | deprecated                 |
| `ZSTD_compressSequences`                   |                                   |                            |
| `ZSTD_compressStream`                      | `zstd_compress_stream`            | deprecated                 |
| `ZSTD_compressStream2`                     | `zstd_compress_stream2`           |                            |
| `ZSTD_compressStream2_simpleArgs`          |                                   |                            |
| `ZSTD_compress_advanced`                   |                                   | deprecated                 |
| `ZSTD_compress_usingCDict`                 |                                   |                            |
| `ZSTD_compress_usingCDict_advanced`        |                                   | deprecated                 |
| `ZSTD_compress_usingDict`                  |                                   |                            |
| `ZSTD_copyCCtx`                            |                                   | deprecated                 |
| `ZSTD_copyDCtx`                            |                                   | deprecated                 |
| `ZSTD_createCCtx`                          | `zstd_create_c_ctx`               |                            |
| `ZSTD_createCCtxParams`                    |                                   |                            |
| `ZSTD_createCCtx_advanced`                 |                                   |                            |
| `ZSTD_createCDict`                         |                                   |                            |
| `ZSTD_createCDict_advanced`                |                                   |                            |
| `ZSTD_createCDict_advanced2`               |                                   | deprecated                 |
| `ZSTD_createCDict_byReference`             |                                   |                            |
| `ZSTD_createCStream`                       | `zstd_create_c_stream`            |                            |
| `ZSTD_createCStream_advanced`              |                                   |                            |
| `ZSTD_createDCtx`                          | `zstd_create_d_ctx`               |                            |
| `ZSTD_createDCtx_advanced`                 |                                   |                            |
| `ZSTD_createDDict`                         |                                   |                            |
| `ZSTD_createDDict_advanced`                |                                   | deprecated                 |
| `ZSTD_createDDict_byReference`             |                                   |                            |
| `ZSTD_createDStream`                       | `zstd_create_d_stream`            |                            |
| `ZSTD_createDStream_advanced`              |                                   |                            |
| `ZSTD_createThreadPool`                    |                                   |                            |
| `ZSTD_dParam_getBounds`                    | `zstd_d_param_get_bounds`         |                            |
| `ZSTD_decodingBufferSize_min`              |                                   |                            |
| `ZSTD_decompressBegin_usingDDict`          |                                   |                            |
| `ZSTD_decompressBegin_usingDict`           |                                   |                            |
| `ZSTD_decompressBegin`                     |                                   |                            |
| `ZSTD_decompressBlock`                     |                                   | deprecated                 |
| `ZSTD_decompressBound`                     | `zstd_decompress_bound`           |                            |
| `ZSTD_decompressContinue`                  |                                   |                            |
| `ZSTD_decompressDCtx`                      | `zstd_decompress_d_ctx`           |                            |
| `ZSTD_decompressStream`                    | `zstd_decompress_stream`          |                            |
| `ZSTD_decompressStream_simpleArgs`         |                                   |                            |
| `ZSTD_decompress_usingDDict`               |                                   |                            |
| `ZSTD_decompress_usingDict`                |                                   |                            |
| `ZSTD_decompress`                          | `zstd_decompress`                 |                            |
| `ZSTD_decompressionMargin`                 |                                   |                            |
| `ZSTD_defaultCLevel`                       | `zstd_default_c_level`            |                            |
| `ZSTD_endStream`                           | `zstd_end_stream`                 | deprecated                 |
| `ZSTD_estimateCCtxSize`                    |                                   |                            |
| `ZSTD_estimateCCtxSize_usingCCtxParams`    |                                   |                            |
| `ZSTD_estimateCCtxSize_usingCParams`       |                                   |                            |
| `ZSTD_estimateCDictSize`                   |                                   |                            |
| `ZSTD_estimateCDictSize_advanced`          |                                   |                            |
| `ZSTD_estimateCStreamSize`                 |                                   |                            |
| `ZSTD_estimateCStreamSize_usingCCtxParams` |                                   |                            |
| `ZSTD_estimateCStreamSize_usingCParams`    |                                   |                            |
| `ZSTD_estimateDCtxSize`                    |                                   |                            |
| `ZSTD_estimateDDictSize`                   |                                   |                            |
| `ZSTD_estimateDStreamSize`                 |                                   |                            |
| `ZSTD_estimateDStreamSize_fromFrame`       |                                   |                            |
| `ZSTD_findDecompressedSize`                | `zstd_find_decompressed_size`     |                            |
| `ZSTD_findFrameCompressedSize`             | `zstd_find_frame_compressed_size` |                            |
| `ZSTD_flushStream`                         | `zstd_flush_stream`               | deprecated                 |
| `ZSTD_frameHeaderSize`                     |                                   |                            |
| `ZSTD_freeCCtxParams`                      |                                   |                            |
| `ZSTD_freeCCtx`                            | `zstd_free_c_ctx`                 | wrapper                    |
| `ZSTD_freeCDict`                           |                                   |                            |
| `ZSTD_freeCStream`                         | `zstd_free_c_stream`              | wrapper                    |
| `ZSTD_freeDCtx`                            | `zstd_free_d_ctx`                 | wrapper                    |
| `ZSTD_freeDDict`                           |                                   |                            |
| `ZSTD_freeDStream`                         | `zstd_free_d_stream`              | wrapper                    |
| `ZSTD_freeThreadPool`                      |                                   |                            |
| `ZSTD_generateSequences`                   |                                   | deprecated                 |
| `ZSTD_getBlockSize`                        |                                   | deprecated                 |
| `ZSTD_getCParams`                          |                                   |                            |
| `ZSTD_getDecompressedSize`                 | `zstd_get_decompressed_size`      | deprecated                 |
| `ZSTD_getDictID_fromCDict`                 |                                   |                            |
| `ZSTD_getDictID_fromDDict`                 |                                   |                            |
| `ZSTD_getDictID_fromDict`                  |                                   |                            |
| `ZSTD_getDictID_fromFrame`                 |                                   |                            |
| `ZSTD_getErrorName`                        | `zstd_get_error_name`             | wrapper                    |
| `ZSTD_getFrameContentSize`                 | `zstd_get_frame_content_size`     |                            |
| `ZSTD_getFrameHeader`                      |                                   |                            |
| `ZSTD_getFrameHeader_advanced`             |                                   |                            |
| `ZSTD_getFrameProgression`                 |                                   |                            |
| `ZSTD_getParams`                           |                                   |                            |
| `ZSTD_initCStream`                         | `zstd_init_c_stream`              |                            |
| `ZSTD_initCStream_advanced`                |                                   | deprecated                 |
| `ZSTD_initCStream_srcSize`                 |                                   | deprecated                 |
| `ZSTD_initCStream_usingCDict`              |                                   | deprecated                 |
| `ZSTD_initCStream_usingCDict_advanced`     |                                   | deprecated                 |
| `ZSTD_initCStream_usingDDict`              |                                   | deprecated                 |
| `ZSTD_initCStream_usingDict`               |                                   | deprecated                 |
| `ZSTD_initDStream`                         | `zstd_init_d_stream`              |                            |
| `ZSTD_initDStream_usingDDict`              |                                   | deprecated                 |
| `ZSTD_initDStream_usingDict`               |                                   | deprecated                 |
| `ZSTD_initStaticCCtx`                      |                                   |                            |
| `ZSTD_initStaticCDict`                     |                                   |                            |
| `ZSTD_initStaticCStream`                   |                                   |                            |
| `ZSTD_initStaticDCtx`                      |                                   |                            |
| `ZSTD_initStaticDDict`                     |                                   |                            |
| `ZSTD_initStaticDStream`                   |                                   |                            |
| `ZSTD_insertBlock`                         |                                   | deprecated                 |
| `ZSTD_isError`                             | `zstd_is_error`                   | wrapper, returns _logical_ |
| `ZSTD_isFrame`                             |                                   |                            |
| `ZSTD_isSkippableFrame`                    |                                   |                            |
| `ZSTD_maxCLevel`                           | `zstd_max_c_level`                |                            |
| `ZSTD_mergeBlockDelimiters`                |                                   |                            |
| `ZSTD_minCLevel`                           | `zstd_min_c_level`                |                            |
| `ZSTD_nextInputType`                       |                                   |                            |
| `ZSTD_nextSrcSizeToDecompress`             |                                   |                            |
| `ZSTD_readSkippableFrame`                  |                                   |                            |
| `ZSTD_registerSequenceProducer`            |                                   |                            |
| `ZSTD_resetCStream`                        |                                   | deprecated                 |
| `ZSTD_resetDStream`                        |                                   | deprecated                 |
| `ZSTD_sequenceBound`                       |                                   |                            |
| `ZSTD_sizeof_CCtx`                         |                                   |                            |
| `ZSTD_sizeof_CDict`                        |                                   |                            |
| `ZSTD_sizeof_CStream`                      |                                   |                            |
| `ZSTD_sizeof_DCtx`                         |                                   |                            |
| `ZSTD_sizeof_DDict`                        |                                   |                            |
| `ZSTD_sizeof_DStream`                      |                                   |                            |
| `ZSTD_toFlushNow`                          |                                   |                            |
| `ZSTD_versionNumber`                       | `zstd_version_number`             |                            |
| `ZSTD_versionString`                       | `zstd_version_string`             | wrapper                    |
| `ZSTD_writeSkippableFrame`                 |                                   |                            |
