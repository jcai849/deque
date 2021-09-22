deque <- function() {
        deque <- list()
        structure(function(..., sideeffect, value, element) {
            returnsvalue <- !missing(value)
            opelement <- !missing(element)
            value <- if (returnsvalue) value(deque, ...) else NULL
            deque <<- if (opelement) sideeffect(deque, element, ...) else sideeffect(deque, ...)
            if (returnsvalue) value else invisible(value) 
        },
        class="deque")
}

front <- function(x, ...) UseMethod("front")
shift <- function(x, ...) UseMethod("shift")
unshift <- function(x, y, ...) UseMethod("unshift")

back <- function(x, ...) UseMethod("back")
pop <- function(x, ...) UseMethod("pop")
push <- function(x, y, ...) UseMethod("push")

front.list <- function(x, ...) if (length(x)) x[[length(x)]] else NULL
shift.list <- function(x, ...) if (length(x)) x[-length(x)] else x
unshift.list <- function(x, y, ...) c(x, list(y))

back.list <- function(x, ...) if (length(x)) x[[1]] else NULL
pop.list <- function(x, ...) if (length(x)) x[-1] else x
push.list <- function(x, y, ...) c(list(y), x)

front.deque <- function(x, ...) x(..., sideeffect=identity, value=front)
shift.deque <- function(x, ...) x(..., sideeffect=shift, value=front)
unshift.deque <- function(x, y, ...) x(..., sideeffect=unshift, element=y)

back.deque <- function(x, ...) x(..., sideeffect=identity, value=back)
pop.deque <- function(x, ...) x(..., sideeffect=pop, value=back)
push.deque <- function(x, y, ...) x(..., sideeffect=push, element=y)

`[.deque` <- function(x, i) as.list(x)[i]
length.deque <- function(x) length(as.list(x))
as.list.deque <- function(x, ...) get("deque", environment(x))
format.deque <- function(x, ...) c("Deque:", length(as.list(x)), "Elements")
print.deque <- function(x, ...) cat(format(x), "\n")
str.deque <- function(object, ...) {
        l <- as.list(object)
        print(object)
        if (length(l))
                cat(c("[ Elements listed from back to front ]"), capture.output(str(l))[-1], sep="\n")
}
