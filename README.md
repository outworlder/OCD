# OCD

OCD is an egg that continuously scans the filesystem for changes and, if any are detected, will run a specified command. By default, it looks for Scheme files, then runs a makefile.

This behaviour can be customized with a .ocdrc file.

# .ocdrc

This file should be placed at the root of the project that it should operate on.

The following is a list of parameters available for customization (so far):

* _ocd-root-directory_
   The directory OCD will begin with. Note that it will recursively scan for files inside this subdirectory. Also note, the recursive scan is currently broken. Default is the current directory.
* _ocd-delay_
   Delay between checks (default is 2 seconds). This will be made irrelevant if a notification library is used in the future
* _ocd-run-command_
   The command to be run. Defaults to "make"
* _ocd-filename-filter_
   List of 'globs' to look for. Default is '("*.scm")

# TODO

  - Add support for fsevent or inotify, in order to improve performance (stop polling!)
  - Document the .ocdrc file
  - Create a default .ocdrc generator
  - Fix filesystem scanning bugs
  - Upload it to Chicken's official repository