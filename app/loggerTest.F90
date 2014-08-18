program loggerTest
use LoggerModule
type(logger) :: log
integer, parameter :: LOGGING_LEVEL = DEBUG_LOGGING_LEVEL
call New(log, LOGGING_LEVEL)
call LogMessage(log, DEBUG_LOGGING_LEVEL,'Hello world')
end program loggerTest
