# data-files-gen

This little utility helps you generate the data-files Cabal field if you have lots of files to include.

You pass it the directory you want to include, and it will generate the proper inclusions lines according to Cabal's rules. So for each file extension, it will generate `dir/*.ext`, and it will recurse into sub directories.

You can exclude some directories by name via the `-d` option (you can use it multiple times), and exclude extensions via the `-e` option.

You can add a specific number of spaces at the start of each generated line with the `-i` option.
