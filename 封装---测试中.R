#########################
#https://zhuanlan.zhihu.com/p/121003243
#https://zhuanlan.zhihu.com/p/142652869
##########################

#devtools::install_github("chasemc/electricShine")
setwd("/home/dev1/dev/rjh-Metastasis")

library(electricShine)
buildPath <- tempdir()
 

my_package <-  system.file("demoApp", package = "electricShine") 
#my_package <- getwd()
electricShine::electrify(app_name = "demo_App", 
                         short_description = "My demo application",
                         semantic_version = "1.0.0", 
                         build_path = getwd(), 
                         mran_date = "2021-05-20",
                         #cran_like_url = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/", # 改成清华镜像
                         function_name = "run_app", 
                         local_package_path = my_package, 
                         package_install_opts = list(type = "binary"),
                         nodejs_path = file.path("D:/Software/node-v10.16.0-win-x64/node-v10.16.0-win-x64")) # 改成自己下载的nodejs路径
