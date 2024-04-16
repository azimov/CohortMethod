# Copyright 2024 Observational Health Data Sciences and Informatics
#
# This file is part of CohortMethod
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

.getRedisCon <- function() {
  config <- getOption("redis.config", default = redux::redis_config())
  if (!redux::redis_available(config))
    stop("Redis cache unavailable")
  return(redux::hiredis(config))
}

saveRedis <- function(object, key) {
  con <- .getRedisCon()
  bin <- redux::object_to_bin(object)
  con$SET(key, bin)
  invisible(TRUE)
}

readRedis <- function(key) {
  con <- .getRedisCon()
  bin <- con$GET(key)
  if (is.null(bin))
    return(NULL)

  return(redux::bin_to_object(bin))
}

redisKeyExists <- function (...) {
  con <- .getRedisCon()
  # allows vector input
  keys <- unlist(lapply(c(...), con$KEYS))
  return(c(...) %in% keys)
}

saveResultObject <- function(...) {
  if (getOption("use.redsis.cache", default = FALSE)) {
    saveRedis(...)
  } else {
    saveRDS(...)
  }
}

readResultObject <- function(...) {
  if (getOption("use.redsis.cache", default = FALSE)) {
    readRedis(...)
  } else {
    readRDS(...)
  }
}

objectExists <- function(objectKey) {
  if (getOption("use.redsis.cache", default = FALSE)) {
    redisKeyExists(objectKey)
  } else {
    return(file.exists(objectKey))
  }
}

deleteRedisCache <- function(keys = "*") {
  con <- .getRedisCon()
  lapply(keys, con$DEL)
}