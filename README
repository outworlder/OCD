* OCD

OCD is an egg that continuously scans the filesystem for changes and, if any are detected, will run a specified command. By default, it looks for Scheme files, then runs a makefile.

This behaviour can be customized with a .ocdrc file.

* ocdrc

The following is a list of parameters available for customization (so far):

** ocd-rood-directory
   The directory OCD will begin with. Note that it will recursively scan for files inside this subdirectory. Also note, the recursive scan is currently broken. Default is the current directory.
** ocd-delay
   Delay between checks (default is 2 seconds). This will be made irrelevant if a notification library is used in the future
** ocd-run-command
   The command to be run. Defaults to "make"
** ocd-filename-filter
   List of 'globs' to look for. Default is '("*.scm")
   
* TODO

  - Add support for fsevent or inotify, in order to improve performance (stop polling!)
  - Document the ocdrc file
  - Upload it to Chicken's official repository