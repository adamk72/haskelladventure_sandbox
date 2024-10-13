# TextAdventure

## Getting it running:

- `stack build`
- `stack run TextAdventure-exe -- --help`

## Todos

- [x] Pull list of adventures from a text file and initially display on screen.
- [x] Update help text with list of adventures so that --help shows them.
- [x] Allow user to input one of those adventure as a command line argument and then display content from a second file associated with that name.
- [] (Low) Add a more complete description of the adventures in the help.
- [x] Refactor out all of the redundant uses of delimiters and column width for easier reading.
- [x] Clean up where Dummy and Nightmare will probably clash.
- [x] Basic: Have it pickup on ":q", so I can stop getting snagged when I type ":q" every time I try to exit.
- [] Advanced: Create a ":" prepended meta-vocabulary (not sure it's needed at this point)
- [] Convert Adventures to JSON files for ingestion.
  - [x] Get aeson lib working by pulling in tiles to help commands from JSON files.
  - [] Pull json data from _separate_ files (one for D and one for N) and then add to help files.
  - [] Start with list of verbs/nouns and such since they are generic, and have them added to the existing code base as an input.
- [] Super advanced: implement Lenses.
