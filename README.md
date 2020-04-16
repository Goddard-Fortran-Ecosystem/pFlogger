- [pFlogger - The parallel Fortran logger](#pflogger---the-parallel-fortran-logger)
- [MPI extensions](#mpi-extensions)
  * [Optional default global communicator](#optional-default-global-communicator)
  * [Locks dictionary](#locks-dictionary)
    + [Options](#options)
  * [MpiFormatter](#mpiformatter)
  * [MPI options for handlers](#mpi-options-for-handlers)
  * [MpiFilter](#mpifilter)
    + [MpiFilter Options](#mpifilter-options)
  * [MPI options for loggers](#mpi-options-for-loggers)
    + ["comm"](#-comm-)
    + ["root_level"](#-root-level-)
- [Performance considerations](#performance-considerations)
- [Advanced features](#advanced-features)

<small><i><a href='http://ecotrust-canada.github.io/markdown-toc/'>Table of contents generated with markdown-toc</a></i></small>

# pFlogger - The parallel Fortran logger

pFlogger is a Fortran adaptation of [Python's wonderful logging package](https://docs.python.org/3/howto/logging.html).
The main differences are:
   1. pFlogger has a smaller set of handler classes
   2. pFlogger provides several MPI extensions
   3. Format strings in pFlogger use a slightly different syntax that is based upon Fortran edit descriptors.
   
   
# MPI extensions

## Optional default global communicator

By default, pFlogger runs under ```MPI_COMM_WORLD``` as a convenience.   But applications are encouraged to use
the optional "comm" dummy argument in ```load_config()``` to specify the communicator under which pFlogger runs.  
(Pflogger does not send any messages involving tags, and thus does not currently dulpicate the communicator.)    

In the discussions below, the term "global communitator" refers to the communicator specified in ```load_config()``` 
or ```MPI_COMM_WORLD``` if omitted.   Even where MPI_COMM_WORLD is explicitly provided in a config file, it is 
translated into this global communicator.


## Locks dictionary

Config files can have a section for locks that are used to manage messages from multiple processes to a shared file.  


### Options
* "class"  currently only MpiLock is supported.  In theory other types of lock subclasses could be created, e.g., for a given filesystem.
But unfortunately, robust locks for NFS are not possible.
* "comm" name the communicator to which the lock will apply.   Built in options are MPI_COMM_WORLD and COMM_LOGGER. 
Both indicate the global communicator (see above), but the latters is considered a best practice.  Additional runtime communicators are possible.  
See Advanced Features

The default is the global communicator for class MpiLock, but users are encourages to explicit confirm.

An example entry could be:
```yaml
...

locks:
    class: MpiLock
    comm:  COMM_LOGGER
...
```





## MpiFormatter

## MPI options for handlers

The handler block in a pFlogger config file permits the following additional entries:
* "comm": specify the communicator (see Advanced Options).   
The default is global communicator.
* "rank_prefix": specify the keyword to be filled my the rank in the MpiFormatter and/or filename.  
The default is 'mpi_rank'.
* "size_prfix": specify the keyword to be filled by the size (npes) in the MpiFormatter and or filename.  
The default is 'mpi_size'.
* "lock":  selects a lock defined in the locks section.  Handler and communicator should be consistent.

Note that the existing "filname" option for a FileHandler can include formatting terms to yield unique file names by process.
For example, if a handler is configured as:
```yaml
   mpi_handler:
      class: MpiFileHandler
      formatter: basic
      filename:  debug_%(rank_prefix)i3.3~.log
      comm: COMM_LOGGER     
      level: DEBUG
```
Then process 0 will open the file "debug_000.log", process 1 will open the file "debug_001.log" and so forth. 
In such cases "lock:" should not be used and would only serve to slow execution.

## MpiFilter

```MpiFilter``` is a Filter subclass that only permits messages to be emitted on a single process of a communicator.  
In many instances, users may prefer to use the more nuanced root_level option for loggers that provides similar functionality.


### MpiFilter Options
* "comm" is used to override the default global communicator
* "root" is used to override the default choice of rank = 0 



## MPI options for loggers

### "comm"

Used to override the default global communicator for the logger.  
Logger objects only use the communicator to determine the root process for the "root_level" option below.

### "root_level"

A common situation is where a given message would have virtually identical content across all MPI processes.  
Rather than protecting the log message within a conditional on the process rank, users can instead spaecify the "root_level" 
option on a given logger to provide a different severity threshold for the root process.  
The usual "level" option still applies to all other processes.   For example with a logger declared as
```yaml
logger_a:
   level: WARNING
   root_level: INFO
```
INFO messages would only be emitted on the root process while WARNING (and above) messages would be emitted on all processes.


# Performance considerations
  
  A considerable amount of effort has gone into optimizing the performance of pFlogger, but users may
  still need to guard debugging messages inside of ```#ifdef``` statements in performance critical sections
  of their code.   Also note that the performance varies widely with compiler vendor.
  
  pFlogger also provides a FastFormatter subclass that trades flexibility of annotations for improved performance.
  
# Advanced features

As with Python's logger, the config file can be supplemented with an "extra" dictionary that contains runtime information.  
This can be used for example to allow subcommunicators and IO units to be referenced in the config file.   
Unfortunately this is not always as useful as it might at first appear because often such things are
not yet known at the time when the logger is initialized.   Future releases of pFlogger may include some support for lazy initialization to sidestep such difficulties.

One nice option with named subcommunicators is to use the ranks in those subcommunicators in formats and/or file names.  
E.g., if an application manages a pool of 1000 processes in a 10 x 10 x 10 topology, a format indicating
process (3,4,1) might be much more informative than the global rank "143".  (OK - the conversion would be much more confusing if I'd 
chosen a topology that did not involve factors of 10.)



