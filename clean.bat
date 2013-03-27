:: Erases all compiled files
@echo off

SET LIBS=(shared worker_server controller apps\inverted_index apps\word_count apps\game_of_life apps\dna_sequencing)

FOR %%x IN %LIBS% DO (
  FOR /D %%y IN (*.cmo *.cmi *.cma *.exe) DO (
    DEL /F "%%y"
  )
)
