# Copyright (C) 2019 Seven Bridges Genomics Inc. All rights reserved.
#
# This document is the property of Seven Bridges Genomics Inc.
# It is considered confidential and proprietary.
#
# This document may not be reproduced or transmitted in any form,
# in whole or in part, without the express written permission of
# Seven Bridges Genomics Inc.

navbarPage(
  id = nav_id,

  windowTitle = "TV Suite",


  title = div(
    img(src = "logo-new.png", height='40', style = "margin:-10px -10px -10px -10px;"),
    br(), HTML("&emsp;&emsp;&emsp;&emsp;&emsp;")
  ),
  source(file.path("ui", "ui-home.R"), local = TRUE)$value,

  source(file.path("ui", "ui-tv.R"), local = TRUE)$value,
  source(file.path("ui", "ui-validate.R"), local = TRUE)$value,
  source(file.path("ui", "ui-help.R"), local = TRUE)$value

)
