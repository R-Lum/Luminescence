# Silence output and warnings
#
# Surround with this function the code that is printing out to the console,
# such as in:
# SW({
# template_DRAC(preset = "DRAC-example_quartz")
# })
SW <- function(expr) capture.output(suppressMessages(suppressWarnings(expr)))
