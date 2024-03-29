:: Build script for Map Reduce
@echo off

SET UTIL_SOURCES=shared\hashtable.mli shared\hashtable.ml shared\util.mli shared\util.ml

SET SHARED_SOURCES=shared\thread_pool.mli shared\thread_pool.ml shared\connection.mli shared\connection.ml shared\protocol.mli shared\protocol.ml

SET WS_SOURCES=worker_server\program.mli worker_server\program.ml worker_server\worker.mli worker_server\worker.ml worker_server\worker_server.mli worker_server\worker_server.ml

SET CONTROLLER_SOURCES=controller\worker_manager.mli controller\worker_manager.ml controller\map_reduce.mli controller\map_reduce.ml

SET APP_SOURCES=apps\inverted_index\inverted_index.ml apps\word_count\word_count.ml apps\game_of_life\game_of_life.ml apps\dna_sequencing\dna_sequencing.ml

:: Builds util.cma
echo Building util.cma...
ocamlc -o shared\util.cma -a -I shared %UTIL_SOURCES%

:: Builds applications
echo Building applications...
ocamlc -c -thread -I shared -I controller %SHARED_SOURCES% %UTIL_SOURCES% %APP_SOURCES%

:: Builds worker server
echo Building worker server...
ocamlc -thread -o worker_server.exe -I shared -I worker_server dynlink.cma str.cma unix.cma threads.cma util.cma %SHARED_SOURCES% %WS_SOURCES%

:: Builds Controller
echo Building controller...
ocamlc -thread -o controller.exe -I shared -I controller -I apps\inverted_index -I apps\word_count -I apps\game_of_life -I apps\dna_sequencing dynlink.cma unix.cma threads.cma str.cma util.cma %SHARED_SOURCES% %CONTROLLER_SOURCES% controller\main.ml
echo All done! Make certain the above code compiled!

pause
