cmake_minimum_required(VERSION 3.4.1)

#           name      type   files ...
add_library(template  SHARED template.c)
add_library(app       SHARED app.c)

#            name     ndk-library
find_library(log-lib  log)

#                     target   deps ...
target_link_libraries(app      ${log-lib})
target_link_libraries(template ${log-lib} app)