# (DRAFT) pFLogger: A Parallel Fortran Logging Framework for HPC Applications

## Background

In High Performance Computing (HPC) applications, the use of text-base
messages (for instance using `print` statements) can be cumbersome.
The messages may not be organized enough to contain critical information 
developers and users need to understand the behavior of their applications.
The typical problems we may encounter are:

- Important messages are obscured by a huge set of routine messages.
- Performance
    - Users add a `print` statement in an inner loop or across all processes.
- Anonymity – important message of unknown origin
    - Which process
    - Which software component
    - Which line in a file
- Loss of productivity
    - Recompile to activate low-level debug diagnostics.
- Complexity
    - Print statements are executed only if preprocessing directives are set at compilation.

In HPC software running on multi-core platforms, we need to have
a framework that facilitates the creation of text-based messages
providing useful information on the behavior of the software so as 
to help code developers and users debug and track down errors systematically.
The framework needs to be able to:

- Route warnings and errors to prominent location
    - And profiler data
- Have the ability to suppress low severity (”debugging”) messages
- Not include duplicate messages on sibling processes
- Be effective with a single or multiple cores
- Annotate messages with:
    - Time stamp
    - Process rank
    - Software component
    - Other application-specific metadata

All these features need to be done dynamically at run time
(without recompiling the application), 
and need to use to log messages that tell a story on the state of the application.

`pFLogger` (__parallel Fortran logger__)
mimics the Python [`logging` module](https://docs.python.org/3/howto/logging.html)
 by implementing its features to support Fortran HPC applications.
 As `logging`, `pFLogger` enables Fortran code developers and users 
 to better control where, how, and what they log, with much more granularity. 
They can then reduce debugging time, improve code quality, and increase the visibility of their applications.

## Understanding the Python `logging` Module

The Python [`logging` module](https://docs.python.org/3/howto/logging.html)
provides a flexible framework for
producing log messages from Python codes.
It allows applications to configure different log handlers and 
a way of routing log messages to these handlers. 
It is used to monitor applications by tracking and recording events that occur, 
such as errors, warnings, or other important information.
The produced log messages can be formatted to meet different requirements,
customized with timestamps, sent to different destinations (terminal, files), 
and filtered based on their severity.
This monitoring helps diagnose issues, understand runtime behavior, 
and analyze user interactions, offering insights into design decisions.
It can help developers easily identify areas where their code needs 
to be improved or better optimised. 
This leads to faster development cycles, fewer bugs and higher-quality code.  

The main components of the `logging` module are:

- __Loggers__: Expose an interface that an application can use to log messages at run time. 
They also determine which log messages to act upon based upon severity (the default filtering facility). 
Loggers have a hierarchy. On top of the hierarchy is the root logger. 
When a new logger is created, its parent will be set to the root logger. 
A logger has three main components:
    - Propagate: Decides whether a log should be propagated to the logger’s parent.
    - A level: Like the log handler level, the logger level is used to filter out “less important” logs. Except, unlike the log handler, the level is only checked at the “child” logger; once the log is propagated to its parents, the level will not be checked. 
    - Handlers: The list of handlers that a log will be sent to when it arrives to a logger. A log will be broadcast to all handlers once it passes the logger level check.
- __Handlers__: Send the logs created by loggers to their destination. Each log handler has two important fields:
    - A formatter which adds context information to a log.
    - A log level that filters out logs whose levels are lower. 
    
    Popular handlers include:
    - `FileHandler`: For sending log messages to a file.
    - `StreamHandler`: For sending log messages to an output stream like stdout.
    - `MemoryHandler`: For sending messages to a buffer in memory, which is flushed whenever specific criteria are met.
    - `HTTPHandler`: For sending log messages with HTTP.
    - `SysLogHandler`: For sending messages to a Unix syslog daemon, possibly on a remote machine.
- __Filters__: Provide a mechanism to determine which logs are recorded. They are used to perform more advanced filtering than level filtering.
- __Formatters__: Determine the output formatting of log messages. They are used by the Python logging handlers to enhance the information available through `LogRecord` attributes. They are useful to know when the log is sent, where (file, line number, subroutine, etc.), and additional context such as the process.

To start with Python `logging` in an existing workflow: 
1. First, create a logger object using the `basicConfig()` method from the `logging` module. 
2. Then, set up handlers for each type of log message to be captured (e.g., debug messages or errors). 
3.  Finally, add filters and formatters as needed to customize how logs are displayed or stored.  

The logging flow can be summarized in the following diagram:

![fig_flow](https://docs.python.org/3/_images/logging_flow.png)

The `logging` module has six logging levels that are of incremental severity: 

| Level	   | Numeric Value | Description |
| --- | --- | --- |
| __NOTSET__ |	0	| Default level when a new logger is created. Setting the root logger to `NOTSET` logs all messages. For other loggers, `NOTSET` messages are delegated to parent loggers until a level other than `NOTSET` is found. |
| __DEBUG__ |	10	| Messages useful for debugging how an application is running. |
| __INFO__ |	20	| Messages that log standard/expected events. |
| __WARNING__ |	30	| Potentially negative events that may cause abnormal operation or otherwise require attention (such as the use of a deprecated API). |
| __ERROR__ |	40	| High severity messages generally related to an operation failing that does not cause the program to crash or exit. |
| __CRITICAL__ |	50	| Highest severity messages; may cause a program to crash or exit. |

Levels are useful because they provide additional information about the runtime's context. 

To use the toool, a user needs to include the line:

```
import logging
```
 to their Python code and then add statements to create log messages.
Assume that the user doesn't set a specific logging level, 
the following code snippet shows how you can use all the five logging 
levels with the syntax:
```python
logging.debug("A DEBUG Message")
logging.info("An INFO")
logging.warning("A WARNING")
logging.error("An ERROR")
logging.critical("A message of CRITICAL severity")
```
The above statements will produce messages corresponding only to  `warning`, `error`, and `critical`:

```
WARNING:root:A WARNING
ERROR:root:An ERROR
CRITICAL:root:A message of CRITICAL severity
```

This is because the default logging level is `warning` and the code will only print
messages that have levels equal to `warning` or above.
Note also that the default message format is:

```
  <SEVERITY>:<NAME>:<MESSAGE>
```

where `<NAME>` is the name of our logger.
We can customized the messages by creating our own logger (instead of using `root`)
using a function call (`logger = logging.getLogger()`) at the top of the file we want to monitor.


## Understanding pFlogger
As mentioned in the introduction, `pFlogger` attempts to implment the 
features of the Python `logging` module in the context of Fortran HPC applications. 
The challenge for `pFlogger`  is to still be effective in recording 
log messages when several processes are used.
We want to keep the same Python `logging` functionalities (as presented above):

1. _Logging levels_ with log severities.
2. _Log Formatter_ to enrich a log message by adding context information to it.
3. _Log Handler_ to write a log in the console or in a file. 
4. _Logger_ is used to log messages or information we want to see.

`pFlogger` implements those features in Fortran to enrich the development and maintainance of Fortran applications on high performance platforms.
As in Python, we want `pFlogger` log messages to be 
_descriptive_ (provide a useful piece of information), 
_contextual_ (give an overview of the state of the application at a given moment), and
_reactive_ (allow users to take action if something happened).

### How the Key Features are Implemented

As mentioned earlier, `pFlogger` imitates the implementation principles
of the Python `logging` module.
As such, all the main classes of `logging` also appear in `pFlogger`.
The main challenge was to add the MPI extensions and make sure 
the messages from MPI processes could be streamlined and properly logged.
Because `pFlogger` is written purely in Fortran, a huge effort was made
to take advantage of the modern object-oriented features of the language
(Fortran 2003 or above).

The following classes were implemented in `pFlogger`:


#### `Logger` Class is the medium which logging events are conveyed.

Associated with a logger instance is a set of handlers which dispatch
logging events to specific destinations, e.g. STDOUT or a file.
A logger can record messages with various levels of  severity. 
A looger looks at a message and ignores it if the message level 
is less severe than its own level (default is INFO).

Loggers in `pFlogger` maintain the hierachical rule and all of them inherit from the root logger. 
Inheritance is defined by "__.__" (dots), like: __mymodule.this.that__
 is child of __mymodule.this__.

![logger_hierar](https://guicommits.com/content/images/2021/09/logger-inheritance.png)

Image Source: [https://guicommits.com/how-to-log-in-python-like-a-pro/](https://guicommits.com/how-to-log-in-python-like-a-pro/)

To accommodate MPI,  the `LoggerManager` is configured with global communicator (defaults to MPI_COMM_WORLD).
In addition, the `Logger` can be associated with a communicator (defaults to global)
with the `root_level` being the root process of a given communicator.


#### `Handler` Class to determine where log messages will be written.

`Handler` instances are responsible for writing log messages.
They send logs to the appropriate destination (file, STDOUT). 
The  `pFlogger` tool exposes four predefined handlers:

It has four main subclasses:

- `StreamHandler` writes logging events to a stream (STDOUT).
It does not close the stream.

- `FileHandler` writes logging events to an arbitrary file.

- `MpiFileHandler` is similar to the `FileHandler` subclass and allow users
to log events in a unique file per MPI process.

- `RotatingFileHandler` is used for logging to a set of files. 
It switches from one file to the next when the current file reaches a certain size.
By default, the file grows indefinitely. You can specify particular
values of max_bytes and backup_count to allow the file to 
rollover at a predetermined size.

In the context of MPI, we include a lock mechanism by using _MpiLock_ for one-sided MPI communication.
We allow multiple processes to share access to a file.
_MpiFilter_ is emplowed to restrict which processes’ messages are reported.
The `MpiFileHandler` subclass routes messages from each process to separate files.




#### `LogRecord` Class to represent events geing logged.

`LogRecord` instances are created every time something is logged. 
They contain all the information pertinent to the event being logged. 
The  main information passed in is a text message and optional arguments 
which are combined to create the message field of the record.


#### `Formatter` Class to ouput the final message by converting it to the final string.

It specifies the layout of log messages in the final destination.
By default if you do not specify a custom `Formatter` class, 
your log records will contain only the text you provided to the corresponding log methods

Formatters need to know how a `LogRecord` is constructed. 
The Formatter can be initialized with a format string which makes use of
knowledge of the `LogRecord` attributes.
The message format string defines the structure of all the messages that will be written.
The message format string is composed of `LogRecord` attributes, some of which are:


```
 %(name)a            Name of the logger
 %(short_name)a      Short name of the logger
 %(level_number)i    Integer severity level of the message. (DEBUG, INFO, ...)
 %(level_name)a      Text logging level for the message 
                        ("DEBUG", "INFO", "WARNING", "ERROR", "CRITICAL")
 %(asctime)a         Textual time when the LogRecord was created
 %(message)a         The result of record%get_message(), computed just as
                        the record is emitted
 %(line)i            Integer line number of recorded in the record.
 %(file)a            Text file recorded in the record.
 %(basename)a        Text basename of file recorded in the record.
```

The `MpiFormatter` subclass was added to be able to identity the the number of 
available processes and a process rank for annotations.

The following diagram illustrates the flow of a Fortran program that writes a message into a log output stream.

![fig_flow](https://github.com/JulesKouatchou/pFlogger/wiki/fig_pflogger_flow.png)

### Additional Capabilities

Ideally, we also want to implement advanced capabilities such as:

__Subcommunicators__: How to specify in run-time configuration file?
1. Construct communicators prior to initializing framework
2. Build dictionary of named communicators
3. Pass as optional argument to framework configuration step

__Simulation time__: Enable annotation of messages with model’s internal representation of
time/phase information
1. Create a custom procedure that accesses model internal state and returns a dictionary of timerelated fields. E.g. _{‘year’: 2000, ’month’: ’May’, ‘phase’: ‘quality control’}_
2. Set logger global procedure pointer `get_sim_time()` to custom procedure.

### `pFlogger` API

In this section, we describe how to include `pFlogger` calls to record log messages
in applications.

#### Step 1: Import the module
Before the declaration section of your application (or the comonent of interest), you first need to import the `pFlogger` module using:

```fortran
   use pFlogger, only : init_pflogger => initialize
   use pFlogger, only : logging
   use pFlogger, only : Logger
```

In addition to that, you need to include the declaration:

```fortran
   class (Logger), pointer :: lgr
```

#### Step 2: Initialization
`pFlogger` needs to be initialized using the statement:

```fortran
   call init_pflogger()
```

You may also want to provide the name of the YAML configuration file that has the
settings `pFlogger` will use to the log records to be written to the desired destination(s).

```fortran
   call logging%load_file('all_in_one.cfg')
```

#### Step 3: Create the logger and  call logger subroutines
The previous calls are executed at the beginning of the application. 
Here, we focus on the calls in any module, subroutine or function.
We first need to create a logger (required to produce messages) using a  name:

```fortran
   lgr => logging%get_logger('my_section_name')
```

We can create multiple loggers to separate logs for different parts of the application.
Loggers are organized hierarchically and identified by name, a string identified by the dots. 
When a logger is created, it is given a name that identifies its location in the logger hierarchy.

Then, we can now introduce the calls for recording messages:

```fortran
   call lgr%info('Entering the file: %a', trim(FILENAME))
   call lgr%info('Begin info at line: %i3.3 in file: %a', __LINE__,__FILE__)

   call lgr%debug('myId= %i %a', mype, trim(Iam))
   call lgr%debug('Begin debug at line: %i3.3 in file: %a', __LINE__,__FILE__)

   call lgr%warning('Warning: ref_date/time %i %i',ref_date, ref_time)
   call lgr%warning('Begin warning at line: %i3.3 in file: %a', __LINE__,__FILE__)

   call lgr%error('Begin error at line: %i3.3 in file: %a', __LINE__,__FILE__)
```

To record arrays, a special treatement is needed to avoid any compilation issue.
`pFlogger` has a utility function to wrap arrays:

```fortran
   use pFlogger, only: WrapArray
   ...
   call lgr%debug("grid global max= [%3(i0,:,',')~]", WrapArray(counts))
   call lgr%debug("ims= [%1000(i0,:,',')~]", WrapArray(ims))
```
Here, `count` is an array with 3 entries and `ims` is array with an 'unknown' number of entries (it is why we use the number 1000 in the formatting).


#### Step 4: Terminate `pFlogger`
Before the exiting the application, ypu need to include the call:

```fortran
   call finalize()
```

### The Configuration File

Users can create a YAML-like configuration file that will be used by their applications
to determine the message formats and the types of messages to record. 
The file has the following main sections:

#### `schema_version`
This is meant to provide the version of pFlogger that users want to use.
It serves for backward compatibility. 
Currently, there is only one versesion and the setting should be:

```
schema_version: 1
```

#### `locks`
It exercises the MPI lock mechanism which permits access to a target by only one process at a time, so that other processes cannot interfere while communication is in progress. 
In the current version of pFlogger, only the `MPI_COMM_WORLD` communicator can be employed.
In the future, any user's created communicator can be used here.

```
locks:
   mpi:
      class: MpiLock
      comm:  MPI_COMM_WORLD
```

#### `formatters`
The section allow users to define the layout 
(what each each logger to follow when generating a log) 
of the logs that will appear in the various output streams.
It represents a list of formatters that will be used by different handlers to format messages,
Here are three `formatters` options (`basic`, `mpi`, `column`):

```
formatters:
   basic:
      class: Formatter
      format:  '%(name)a~: %(level_name)a~: %(message)a'
   mpi:
      class: MpiFormatter
      format:  '%(mpi_rank)i4.4~: %(name)~: %(level_name)a~: %(message)a'
      comm: MPI_COMM_WORLD
   column:
      class: Formatter
      format: '(%(i)i3.3,%(j)i3.3): %(level_name)'
```
 Note here that we need to specify the `pFlogger` formatter class that will be used to implement the desired ouput format.

#### `handlers`
This section defines the various output streams (standard output, specific files, unique file per process) the log records will be sent to.
It provides the settings specifying where the log messages will be saved. 
We need to provide the name of the output stream (as a subsection) and include the settings:
- `class`: for the `pFlogger` class to be considered for the stream.
- `formatter`: any of the appropriate formatter denied in the section `formatters`.
- `unit`: can be the Fortran OUTPUT_UNIT or ERROR_UNIT
- `level`: the (lowest) logger level to apply to the stream
- `filename`: name of the file the log records will be written into. We can have a setting where each MPI process has its own log file.
- `comm`: the MPI communicator to use
- `lock`: In the context where all the MPI processes write to the same file, it is important to make sure that there is no conflict.

```
handlers:
   console:
      class: streamhandler
      formatter: basic
      unit: OUTPUT_UNIT
      level: DEBUG

   warnings:
      class:  FileHandler
      filename: warnings.log
      lock: mpi
      level: WARNING
      formatter: basic

   infos:
      class:  FileHandler
      filename: infos.log
      lock: mpi
      level: INFO
      formatter: mpi

   debugs:
      class:  FileHandler
      filename: debugs.log
      lock: mpi
      level: DEBUG
      formatter: mpi

   errors:
      class:  StreamHandler
      formatter: basic
      unit: ERROR_UNIT
      level: ERROR

   mpi_shared:
      class: FileHandler
      filename: allPEs.log
      formatter: mpi
      comm: MPI_COMM_WORLD
      lock: mpi
      rank_keyword: rank
      level: INFO

   mpi_debug:
      class: MpiFileHandler
      formatter: basic
      filename:  debug_%(rank)i3.3~.log
      comm: MPI_COMM_WORLD
      rank_prefix: rank
      level: INFO
```
Note that in the `mpi_debug` sub-section, each process records the log 
messages in its own file. 
The logger `level` applies to all the processes. 
If instead `rootlevel` is instead used, it will be for the root process 
of the communicator.

#### `root` 
It is meant to represent the root logger of the application.
Here, we select the logger output streams that will be used 
based on the settings in the `handlers` section.
It has the members variables:

- `parallel`
- `handlers`: the list selected handler names defined in the `handlers` section of the configuration file.
- `level`: name of the lowest level that will be considered for all the selected streams in `handlers`

```
root:
   parallel: true
   handlers: [console,warnings,debugs,infos]
   level: DEBUG
```

It is recommended that users only attach each handler to one logger and rely on propagation to apply handlers to the appropriate child loggers. This means that if users have a default logging configuration that they want all of loggers to pick up, they need add it to a parent logger (such as the root logger), rather than applying it to each lower-level logger.

#### `loggers`:
Here we list all the loggers that are instantiated in the code through the call:

```fortran
    call logging%get_logger(logger_name)
```
In this section, we provide the names of the loggers associated with the code sections
 we want to monitor.
 Each logger is defined with its own configuration.

```
loggers:

   MAPL.GENERIC:
      parallel: false
      comm: MPI_COMM_WORLD
      level: INFO

   parallel:
      parallel: true
      handlers: [mpi_debug,mpi_shared]
      lock: mpi
      propagate: true
      level: INFO
```

By default, all created loggers will pass the log events to the handlers of
the parent logger, in addition to any handlers attached to the created logger. 
You can deactivate this by setting `propagate: false`. 
Sometimes when you wonder why you don't see log messages from another module, then this property may be the reason.




### Compiling your Application with `pFlogger`



### Example

#### Sample Code

Consider the following Fortran code (contained in a file name _complexMpi.F90_). 
It shows how to include `pFlogger` statements to record log messages.
There are a main program, and two supporting subroutines (_sub\_A_ and _sub\_B_),
all of them having different types of annotations.

- Main program
    - Is expected to read the configuration file _all\_in\_one.cfg_
    - Has one logger, `main` with associated associated message of level INFO.
- Subroutine _sub\_A_
    - Has two loggers 
         - `main.A` with associated messages of levels INFO, DEBUG and WARNING.
         - 'parallel.A` with associated messages of levels INFO and DEBUG. 
- Subroutine _sub\_B_
    - Has two loggers 
         - `main.B` with associated messages of levels INFO, DEBUG and ERROR.
         - 'parallel.B` with an associated message of level INFO. 

```fortran
subroutine sub_A()
   use pflogger

   integer :: i
   class (Logger), pointer :: log
   class (Logger), pointer :: plog

   log => logging%get_logger('main.A')
   plog => logging%get_logger('parallel.A')

   call log%info('at line: %i3.3 in file: %a', __LINE__,__FILE__)
   call log%debug('inside sub_A')
   call plog%info('at line: %i3.3 in file: %a', __LINE__,__FILE__)
   call plog%debug('inside sub_A')

   call log%warning('empty procedure')
   call log%info('at line: %i3.3 in file: %a', __LINE__,__FILE__)

end subroutine sub_A

subroutine sub_B()
   use pflogger

   integer :: i
   class (Logger), pointer :: log
   class (Logger), pointer :: plog

   log => logging%get_logger('main.B')
   plog => logging%get_logger('parallel.B')

   call log%info('at line: %i3.3 in file: %a', __LINE__,__FILE__)
   call log%debug('inside sub_B')
   call plog%debug('inside sub_B')

   call log%error('this procedure is empty as well')
   call log%info('at line: %i3.3 in file: %a', __LINE__,__FILE__)

end subroutine sub_B

program main
   use pflogger
   implicit none

   integer :: ier
   class (Logger), pointer :: log
   integer :: rc
   integer :: rank

   call mpi_init(ier)
   block
     use mpi
     call mpi_comm_rank(MPI_COMM_WORLD, rank, rc)
   end block
   call initialize() ! init logger

   call logging%load_file('all_in_one.cfg')

   log => logging%get_logger('main')

   call log%info('at line: %i3.3 in file: %a', __LINE__,__FILE__)
   call sub_A()

   call log%info('at line: %i3.3 in file: %a', __LINE__,__FILE__)
   call sub_B()

   call log%info('at line: %i3.3 in file: %a', __LINE__,__FILE__)
   call mpi_finalize(ier)

end program main
```

#### Sample Configuration File

The configuration below is meant to be used by the executable (of the above code)
to determine which messages will be logged and to which destination(s).

```
schema_version: 1

locks:
   mpi:
      class: MpiLock
      comm:  MPI_COMM_WORLD

formatters:
   basic:
      class: Formatter
      format:  '%(name)a~: %(level_name)a~: %(message)a'
   mpi:
      class: MpiFormatter
      format:  '%(mpi_rank)i4.4~: %(name)~: %(level_name)a~: %(message)a'
      comm: MPI_COMM_WORLD
   column:
      class: Formatter
      format: '(%(i)i3.3,%(j)i3.3): %(level_name)'

handlers:
   console:
      class: streamhandler
      formatter: basic
      unit: OUTPUT_UNIT
      level: WARNING

   warnings:
      class:  FileHandler
      filename: warnings.log
      lock: mpi
      level: WARNING
      formatter: basic

   errors:
      class:  StreamHandler
      formatter: basic
      unit: ERROR_UNIT
      level: ERROR

   mpi_shared:
      class: FileHandler
      filename: allPEs.log
      formatter: mpi
      comm: MPI_COMM_WORLD
      lock: mpi
      rank_keyword: rank
      level: DEBUG

   mpi_debug:
      class: MpiFileHandler
      formatter: basic
      filename:  debug_%(rank)i3.3~.log
      comm: MPI_COMM_WORLD
      rank_prefix: rank
      level: DEBUG

root:
   parallel: true
   handlers: [warnings,errors]
   level: WARNING

loggers:

   main:
      parallel: false
      comm: MPI_COMM_WORLD
      level: INFO

   parallel:
      parallel: true
      handlers: [mpi_debug,mpi_shared]
      lock: mpi
      propagate: false
      level: DEBUG

   main.A:
      level: WARNING

   main.B:
      level: INFO

   parallel.A:
      level: WARNING

   parallel.B:
      level: DEBUG
```

#### Log Messages

After running the application with multiple processor, using the above configuration file, we expect the following output streams:

- From __root__:
   - _warnings.log_: for reporting the WARNING logs (or above). 
   - _STDOUT_: for reporting the ERROR logs (or above).
   - Since the __root__ level was set to WARNING, then _warning.log_ file will be produced and the STDOUT will have ERROR logs.
- From __parallel__:
   - _allPEs.log_ for recording all the DEBUG logs (or above) from all the MPI processes.
   - _debug_XXX.log_: (where _XXX_ is the process id) for DEBUG logs (or above) for processor _XXX_.
   - Since the __parallel__ level was set to DEBUG, all the DEBUG logs (and above) will be written in both sets of files.

Assuming that we are using 4 processors to run the application, we will the produce the files:

```
STDOUT
    main.B: ERROR: this procedure is empty as well
    main.B: ERROR: this procedure is empty as well
    main.B: ERROR: this procedure is empty as well
    main.B: ERROR: this procedure is empty as well

warnings.log
    main.A: WARNING: empty procedure
    main.A: WARNING: empty procedure
    main.A: WARNING: empty procedure
    main.A: WARNING: empty procedure
    main.B: ERROR: this procedure is empty as well
    main.B: ERROR: this procedure is empty as well
    main.B: ERROR: this procedure is empty as well
    main.B: ERROR: this procedure is empty as well

allPEs.log
    0003: parallel.B: DEBUG: inside sub_B
    0000: parallel.B: DEBUG: inside sub_B
    0001: parallel.B: DEBUG: inside sub_B
    0002: parallel.B: DEBUG: inside sub_B

debug_000.log
    parallel.B: DEBUG: inside sub_B

debug_001.log
    parallel.B: DEBUG: inside sub_B

debug_002.log
    parallel.B: DEBUG: inside sub_B

debug_003.log
    parallel.B: DEBUG: inside sub_B

```


## References

- [Python Logging HOWTO](https://docs.python.org/3/howto/logging.html)
- [Python Logging Cookbook](https://docs.python.org/3/howto/logging-cookbook.html#logging-cookbook)
- Arfan Sharif, [Python Logging Guide: The Basics](https://www.crowdstrike.com/guides/python-logging/), February 3, 2023.
- Son Nguyen Kim, [Python Logging: An In-Depth Tutorial](https://www.toptal.com/python/in-depth-python-logging)
- Thomas Clune and Carlos Cruz,
     [`pFlogger: The Parallel Fortran LOgging UTility`](https://ntrs.nasa.gov/api/citations/20170011458/downloads/20170011458.pdf), CodeSE17, Denver, CO.
- Thomas L. Clune and Carlos A. Cruz,
    `pFLogger: The parallel Fortran logging framework for HPC applications`,
    Proceedings of the 1st International Workshop on Software Engineering 
    for High Performance Computing in Computational and Data-enabled 
    Science & Engineering, November 2017, Pages 18-21, 
    https://doi.org/10.1145/3144763.3144764
