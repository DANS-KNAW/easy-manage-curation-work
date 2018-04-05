easy-manage-curation-work
===========
[![Build Status](https://travis-ci.org/DANS-KNAW/easy-manage-curation-work.png?branch=master)](https://travis-ci.org/DANS-KNAW/easy-manage-curation-work)


SYNOPSIS
--------

    easy-manage-curation-work list [<easy-datamanager>]
    easy-manage-curation-work assign <easy-datamanager> <UUID>
    easy-manage-curation-work unassign [<easy-datamanager> [<UUID>]]
     
         
ARGUMENTS
--------
   
     Options:
              --help      Show help message
              --version   Show version of this program
        
        Subcommand: list - Lists the current curation tasks.
              --help   Show help message
        
         trailing arguments:
          easy-datamanager (not required)   Datamanager, whose to-be-curated deposits
                                            are listed. If not specified, deposits from
                                            the common curation area are listed.
        ---
        
        Subcommand: assign - Assigns curation task to a datamanager.
              --help   Show help message
        
         trailing arguments:
          easy-datamanager (required)   Datamanager, to whom the deposit will be
                                        assigned.
          uuid (required)               UUID of the deposit to be assigned.
        ---
        
        Subcommand: unassign - Unassigns curation tasks.
              --help   Show help message
        
         trailing arguments:
          easy-datamanager (not required)   Datamanager, from whom the deposit will be
                                            unassigned. If not specified, datamanager is
                                            the current user.
          uuid (not required)               UUID of the deposit to be unassigned. If not
                                            specified, all deposits of the datamanager
                                            are unassigned.
        ---
    
     
DESCRIPTION
-----------

View and assign curation tasks.
     
EXAMPLES
--------

     easy-manage-curation-work list
     easy-manage-curation-work list someDatamanagerId
     easy-manage-curation-work assign someDatamanagerId someUUID
     easy-manage-curation-work unassign someDatamanagerId someUUID
     easy-manage-curation-work unassign someDatamanagerId
     easy-manage-curation-work unassign


INSTALLATION AND CONFIGURATION
------------------------------


1. Unzip the tarball to a directory of your choice, typically `/usr/local/`
2. A new directory called easy-manage-curation-work-<version> will be created
3. Add the command script to your `PATH` environment variable by creating a symbolic link to it from a directory that is
   on the path, e.g. 
   
        ln -s /usr/local/easy-manage-curation-work-<version>/bin/easy-manage-curation-work /usr/bin



General configuration settings can be set in `cfg/application.properties` and logging can be configured
in `cfg/logback.xml`. The available settings are explained in comments in aforementioned files.


BUILDING FROM SOURCE
--------------------

Prerequisites:

* Java 8 or higher
* Maven 3.3.3 or higher

Steps:

        git clone https://github.com/DANS-KNAW/easy-manage-curation-work.git
        cd easy-manage-curation-work
        mvn install
