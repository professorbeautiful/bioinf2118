## From https://csgillespie.github.io/efficientR/programming.html
## 3.6.1 Function closures

stop_watch = function() {
  start_time = stop_time = NULL
  start = function() start_time <<- Sys.time()
  stop = function() {
    stop_time <<- Sys.time()
    difftime(stop_time, start_time)
  }
  list(start = start, stop = stop)
}
watch = stop_watch()
# watch$start()
# watch$stop()
environment(stop_watch)
environment(watch$start)
watch2 = stop_watch()
environment(watch2$start)  
### Each copy has a different environment for its functions.
watch2$start()
watch$stop()
watch2$stop()
