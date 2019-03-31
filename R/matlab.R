#----------------------------------------------------------------------------#
# This file contains three functions:
#
#   - CallMatlab allows you to execute Matlab code from R.
#   - CallCVX allows you to use CVX from R.
#   - CallCVX.varyparam allows you to use CVX for a sequence of problems from R.
#
# See examples.R for usage of these functions.
#----------------------------------------------------------------------------#


#' Function that executes Matlab code
#'
#' Given a string of Matlab code, a list of inputs, and a vector of names of
#' output variables, this function opens Matlab, runs the Matlab code, closes
#' Matlab, and then returns all the output variables. This function assumes that
#' Matlab can be called from the command line using "matlab" (or more generally
#' the string given in the argument matlab.call)
#'
#' This function is pretty low-tech but works well. It begins by writing every
#' variable in inputs to a separate text file named temp_in_<variablename>.txt.
#' A Matlab expression is then formed that does the following:
#'
#' 1) reads in each of the input files; 2) executes matlab.code; 3) writes each
#' output variable to a file named temp_out_<variablename>.txt.
#'
#' CallMatlab uses the R function "system" to call Matlab from the command line.
#' Finally, R reads in each output file and deletes all the files that were
#' created (if delete.temp==TRUE).
#'
#' @param matlab.code A string containing Matlab code. Use semicolons to
#'   separate lines. This function assumes that the Matlab code is valid. The
#'   names of variables used in this string should match those in the inputs
#'   argument.
#' @param inputs List containing all variables that need to be passed to Matlab.
#'   The names of the elements of the list should match the names used in
#'   matlab.code. All variables must be numeric (e.g. matrices, vectors,
#'   scalars)
#' @param output.names Array of names of all variables that should be returned
#'   from Matlab. The names should match those used in matlab.code. All
#'   variables must be numeric (no strings).
#' @param delete.temp Default TRUE. Indicates whether the temporary files that
#'   are created should be deleted.
#' @param norun Default FALSE. Mostly for debugging purposes. Returns the
#'   command that would be run in Matlab without it actually opening Matlab.
#' @param matlab.call How Matlab can be invoked through the "system" command.
#'   Default: "matlab" but even if this is the alias in your default shell,
#'   "system" might use a different shell in which "matlab" is not recognized.
#' @param unique.string a character string added to temporary files. This is
#'   necessary if running in parallel (for example with parallel::mclapply)
#'   since the base::tempfile function may not produce unique file names
#'   otherwise. See help(tempfile).
#'
#' @return A list containing all the outputs that were listed in output.names
#'   with the values they have after matlab.code was executed in Matlab.
#'   \item{command}{The string that is run on the command line.}
#' @author Jacob Bien
#' @examples
#'
#' # 1) example of basic usage
#' x <- rnorm(100)
#' y <- 1:100
#' inputs <- list(x = x, y = y)
#' \dontrun{
#'   out <- CallMatlab("z=x+y", inputs, "z")
#'   out2 <- CallMatlab("z=x+y;x=x+1", inputs, c("z", "x"))
#' }
#'
#' # 2) example where there are no input variables
#' \dontrun{
#' CallMatlab("I = eye(100)", output.names = "I")
#' }
#'
#' @export
CallMatlab <- function(matlab.code, inputs = list(), output.names = NULL,
                       delete.temp = TRUE, norun = FALSE,
                       matlab.call = "matlab", unique.string = "") {
  ## Function that executes Matlab code
  ##
  ## Arguments:
  ##   matlab.code: string containing matlab code
  ##   inputs: list containing all variables referred to in matlab.code.
  ##           The names of the elements of the list should match the name
  ##           used in matlab.code
  ##   ouput.names: array of names of any variables that should be outputted.
  ##                The names should match those used in matlab.code.
  ##   delete.temp: indicates whether the temp files that are created should be
  ##                deleted.
  ##   norun: doesn't call Matlab.  Returns the command that would be run
  ##          in Matlab.
  ##   matlab.call: how Matlab can be invoked through "system" command.
  ##                Default: matlab" but even if this is the alias in your
  ##                default shell, "system" might use a different shell in
  ##                which "matlab" is not recognized.
  ##  unique.string: a character string added to temporary files.
  ##                 This is necessary if running in parallel (for example
  ##                 with parallel::mclapply) since the base::tempfile function
  ##                 may not produce unique file names otherwise.
  ##                 See help(tempfile)
  ##
  ## Returns:
  ##   outputs: list containing all variables that were listed in output.names
  ##            with the values they have after matlab.code was executed
  ##            in Matlab
  ##   command: the string that is run on the command line.
  infiles <- NULL
  outfiles <- NULL
  before <- ""
  if (length(inputs) > 0) {
    for (i in seq(length(inputs))) {
      ## WRITE INPUT VARIABLE TO TEXT FILE
      input.name <- names(inputs)[i]
      if (!is.numeric(inputs[[i]]))
        stop(sprintf("All inputs must be numeric! (Check %s)", input.name))
      ## file <- sprintf("temp_in_%s.txt", input.name)
      file <- tempfile(pattern = sprintf("temp_in_%s_%s.txt", input.name,
                                         unique.string), fileext = ".txt")
      utils::write.table(inputs[[i]],
                         file = file,
                         row.names = FALSE,
                         col.names = FALSE)
      ## FORM MATLAB EXPRESSION TO READ IN THIS FILE
      before <- sprintf(paste0("%s disp('Reading %s into Matlab...'); ",
                               "%s = dlmread('%s');"),
                        before, input.name, input.name, file)
      infiles <- c(infiles, file)
    }
  }
  after <- ";"
  if (!is.null(output.names)) {
    for (out in output.names) {
      ## FORM MATLAB EXPRESSION TO WRITE OUTPUT VARIABLES TO FILE
      ## file <- sprintf("temp_out_%s.txt", out)
      file <- tempfile(pattern = sprintf("temp_out_%s_%s.txt", out,
                                         unique.string),
                       fileext = ".txt")
      after <- sprintf("%s dlmwrite('%s', full(%s), 'precision', '%s10.10f');",
                       after, file, out, "%")
      outfiles <- c(outfiles, file)
    }
  }
  ## PUT TOGETHER FULL MATLAB COMMAND
  if (.Platform$OS.type == "unix")
    command <- sprintf("%s -nodisplay -r \"%s%s%s%s\"",
                       matlab.call, before, matlab.code, after, "exit;")
  else if (.Platform$OS.type == "windows")
    command <- sprintf("%s -wait -nosplash -nodesktop -r \"%s%s%s%s\"",
                       matlab.call, before, matlab.code, after, "exit;")
  else stop("Not a recognized operating system.")
  if (norun) return(command)
  ## EXECUTE COMMAND TO OPEN MATLAB, RUN CODE, AND EXIT
  system(command)

  ## READ MATLAB OUTPUT FILES INTO R
  outputs <- list()
  if (!is.null(output.names)) {
    for (i in seq(length(output.names))) {
      outputs[[output.names[[i]]]] <- drop(as.matrix(
        utils::read.csv(outfiles[i], header = FALSE)))
      colnames(outputs[[output.names[[i]]]]) <- NULL
    }
  }
  if (delete.temp) {
    ## DELETE TEXT FILES THAT WERE CREATED
    files <- paste(c(infiles, outfiles), collapse = " ")
    system(sprintf("rm %s", files))
  }
  outputs[["command"]] <- command

  outputs
}



#' Simple R interface to CVX.
#'
#' This function takes a string containing what you'd normally put between
#' cvx_begin and cvx_end. Returns CVX output.
#'
#' This function is based on \code{CallMatlab}.
#'
#' @param cvx.code String containing call to CVX, i.e. what's inside cvx_begin
#'   and cvx_end
#' @param const.vars List of non-optimization variables used in CVX expression.
#'   Labels of list elements should match what the corresponding variable name
#'   in cvx.code. E.g. for the Lasso, this would be list(y=y, x=x, lam=lam, p=p)
#' @param opt.var.names Array of names of optimization variables. E.g. for the
#'   Lasso, this would be "b"
#' @param setup.dir Directory containing the file cvx_setup. If not needed,
#'   leave NULL.
#' @param norun Default FALSE. Mostly for debugging purposes. Doesn't call
#'   Matlab. Returns the command that would be run in Matlab.
#' @param matlab.call How Matlab can be invoked through the "system" command.
#'   Default: "matlab" but even if this is the alias in your default shell,
#'   "system" might use a different shell in which "matlab" is not recognized.
#' @param cvx.modifiers Optional string of modifiers passed to CVX on same line
#'   as cvx_begin. E.g. "quiet" or "sdp".
#' @param unique.string a character string added to temporary files. This is
#'   necessary if running in parallel (for example with parallel::mclapply)
#'   since the base::tempfile function may not produce unique file names
#'   otherwise. See help(tempfile).
#'
#' @return A list of the optimization variables specified in opt.var.names.
#'   Also,
#'   \item{cvx_optval}{The optimal value as returned by CVX.}
#'   \item{time}{Elapsed time specifically for CVX call (i.e. excludes opening
#'   Matlab, loading data, etc.)}
#' @author Jacob Bien
#' @seealso \code{\link{CallMatlab}}
#' @references M. Grant and S. Boyd. CVX: Matlab software for disciplined convex
#'   programming, version 1.21. http://cvxr.com/cvx, April 2011.
#'
#' M. Grant and S. Boyd. Graph implementations for nonsmooth convex programs,
#' Recent Advances in Learning and Control (a tribute to M. Vidyasagar), V.
#' Blondel, S. Boyd, and H. Kimura, editors, pages 95-110, Lecture Notes in
#' Control and Information Sciences, Springer, 2008.
#' http://stanford.edu/~boyd/graph_dcp.html.
#' @examples
#'
#' # 1) solve both forms of the lasso using CVX
#' n <- 50
#' p <- 10
#' x <- matrix(rnorm(n * p), n, p)
#' beta <- rnorm(p)
#' y <- x %*% beta + 0.1 * rnorm(n)
#' lam <- 2
#' # to call CVX, set setup.dir to be the directory containing "cvx_setup.m"
#' \dontrun{
#' setup.dir <- "change/this/to/your/cvx/directory"
#' lasso <- CallCVX(paste0("variables b(p);",
#'                         "minimize(square_pos(norm(y-x*b,2))/2",
#'                         "+lam*norm(b,1))"),
#'                  const.vars = list(p = p, y = y, x = x, lam = lam),
#'                  opt.var.names = "b", setup.dir = setup.dir)
#'
#' s <- 0.1
#' lasso.boundform <- CallCVX(paste0("variables b(p);",
#'                                   "minimize(norm(y-x*b, 2));",
#'                                   "subject to;",
#'                                   "norm(b,1)<=s"),
#'                            const.vars = list(p = p, y = y, x = x, s = s),
#'                            opt.var.names = "b", setup.dir = setup.dir)
#' }
#' # 2) solve the graphical lasso using CVX
#' n <- 50
#' p <- 10
#' x <- matrix(rnorm(n * p), n, p)
#' S <- cov(x)
#' rho <- 0.1
#' \dontrun{
#' glasso <- CallCVX(paste0("variables Th(p,p);",
#'                          "minimize(-log_det(Th)+trace(S*Th)",
#'                          "+rho*norm(vec(Th),1));",
#'                          "Th==semidefinite(p)"),
#'                   const.vars = list(p = p, S = S, rho = rho),
#'                   opt.var.names = "Th", setup.dir = setup.dir)
#' }
#'
#' @export
CallCVX <- function(cvx.code, const.vars, opt.var.names, setup.dir = NULL,
                    norun = FALSE, matlab.call = "matlab", cvx.modifiers = NULL,
                    unique.string = "") {
  ## Simple R interface to CVX
  ##
  ## Arguments:
  ##   cvx.code: string containing call to CVX, i.e. what's inside cvx_begin
  ##             and cvx_end.
  ##   const.vars: list of non-optimization variables used in CVX expression.
  ##               labels of list elements should be the name of the variable.
  ##   opt.var.names: array of names of optimization variables.
  ##   setup.dir: directory containing the file cvx_setup.  If not needed,
  ##              leave null.
  ##   norun: doesn't call Matlab.  Returns the command that would be run
  ##          in Matlab.
  ##   matlab.call: how Matlab can be invoked through "system" command.
  ##                Default: "matlab" but even if this is the alias in your
  ##                default shell, "system" might use a different shell
  ##                in which "matlab" is not recognized.
  ##   cvx.modifiers: optional string of modifiers passed to CVX on same line
  ##                  as cvx_begin. E.g. "quiet" or "sdp".
  ##   unique.string: a character string added to temporary files. This is
  ##                  necessary if running in parallel (for example with
  ##                  parallel::mclapply) since the base::tempfile function
  ##                  may not produce unique file names otherwise.
  ##                  See help(tempfile).

  ##
  ## Returns:
  ##   cvx_optval (as returned by CVX) and an optimal point found by CVX
  setup <- ""
  if (!is.null(setup.dir)) {
    nc <- nchar(setup.dir)
    if (substr(setup.dir, nc, nc) == "/")
      setup.dir <- substr(setup.dir, 1, nc - 1)
    if (!("cvx_setup.m" %in% list.files(setup.dir)))
      stop("Could not find cvx_setup.m in user-provided \'setup.dir\'.")
    setup <- sprintf("run %s/cvx_setup;", setup.dir)
  }
  if (is.null(cvx.modifiers)) cvx.modifiers <- ""
  # SEEMS TO GET CAUGHT WHEN " * " IS USED.  SO REMOVE SPACES IN THIS CASE
  cvx.code <- gsub(" +[*] +", "*", cvx.code)
  if ("time" %in% opt.var.names)
    stop("'time' is an invalid name for an optimization variable.")
  matlab.code <- sprintf(paste0("%s tStart=tic; ",
                                "cvx_begin %s; %s; cvx_end;",
                                "time=toc(tStart);",
                                "disp(['time elapsed: ',num2str(time)])"),
                         setup, cvx.modifiers, cvx.code)
  CallMatlab(matlab.code, inputs = const.vars,
             output.names = c(opt.var.names, "cvx_optval", "time"),
             norun = norun, matlab.call = matlab.call,
             unique.string = unique.string)
}



#' Simple R interface to CVX to solve sequence of problems
#'
#' This function takes a string containing what you'd normally put between
#' cvx_begin and cvx_end. Returns CVX output. Allows a sequence of problems to
#' be performed that are identical except that a single scalar parameter is
#' varied.
#'
#' This function is very similar to \code{CallCVX}, but allows one to solve a
#' sequence of problems in which only one (scalar) non-optimization variable is
#' varied. The intended use is for when a problem has a tuning parameter, and
#' you want to solve the problem at various values of this parameter. Using this
#' function is much more efficient than wrapping \code{CallCVX} in a loop.
#'
#' @param cvx.code String containing call to CVX, i.e. what's inside cvx_begin
#'   and cvx_end
#' @param const.vars List of non-optimization variables used in CVX expression.
#'   Labels of list elements should match what the corresponding variable name
#'   in cvx.code. E.g. for the Lasso, this would be list(y=y, x=x, lam=lam, p=p)
#' @param tuning.param A list with a single vector (with a name). E.g.,
#'   list(lam=c(0.1, 1, 2)). This is a non-optimization variable used in CVX
#'   expression that you want varied. Each element of this vector is a level of
#'   the parameter. The problem will be solved at each such level.
#' @param opt.var.names Array of names of optimization variables. E.g., for the
#'   Lasso, this would be "beta"
#' @param setup.dir Directory containing the file cvx_setup. If not needed,
#'   leave NULL.
#' @param norun Default FALSE. Mostly for debugging purposes. Doesn't call
#'   Matlab. Returns the command that would be run in Matlab.
#' @param matlab.call How Matlab can be invoked through the "system" command.
#'   Default: "matlab" but even if this is the alias in your default shell,
#'   "system" might use a different shell in which "matlab" is not recognized.
#' @param cvx.modifiers Optional string of modifiers passed to CVX on same line
#'   as cvx_begin. E.g. "quiet" or "sdp".
#' @param unique.string a character string added to temporary files. This is
#'   necessary if running in parallel (for example with parallel::mclapply)
#'   since the base::tempfile function may not produce unique file names
#'   otherwise. See help(tempfile).
#'
#' @return A list of the optimization variables specified in opt.var.names. Each
#'   variable is a list of length equal to the number of problems solved. Also,
#'   \item{cvx_optval}{The optimal values as returned by CVX in each of the
#'   problems.} \item{time}{Elapsed total time specifically for all the CVX
#'   calls (i.e. excludes opening Matlab, loading data, etc.)}
#' @author Jacob Bien
#' @seealso \code{\link{CallCVX}}
#' @references M. Grant and S. Boyd. CVX: Matlab software for disciplined convex
#'   programming, version 1.21. http://cvxr.com/cvx, April 2011.
#'
#' M. Grant and S. Boyd. Graph implementations for nonsmooth convex programs,
#' Recent Advances in Learning and Control (a tribute to M. Vidyasagar), V.
#' Blondel, S. Boyd, and H. Kimura, editors, pages 95-110, Lecture Notes in
#' Control and Information Sciences, Springer, 2008.
#' http://stanford.edu/~boyd/graph_dcp.html.
#' @examples
#'
#' # Solve the lasso at multiple lambda values using CVX
#' n <- 50
#' p <- 10
#' x <- matrix(rnorm(n * p), n, p)
#' beta <- rnorm(p)
#' y <- x %*% beta + 0.1 * rnorm(n)
#' lam <- seq(2, 0.1, length=10)
#' # to call CVX, set setup.dir to be the directory containing "cvx_setup.m"
#' \dontrun{
#' setup.dir <- "change/this/to/your/cvx/directory"
#' lasso <- CallCVX.varyparam(paste0("variables beta(p);",
#'                                   "minimize(square_pos(norm(y-x*beta,2))/2",
#'                                   "+lam*norm(beta,1))"),
#'                            const.vars = list(p = p, y = y, x = x),
#' 		                        tuning.param = list(lam = lam),
#'                            opt.var.names = "beta", setup.dir = setup.dir)
#' }
#'
#' @export
CallCVX.varyparam <- function(cvx.code, const.vars, tuning.param, opt.var.names,
                              setup.dir=NULL, norun=FALSE, matlab.call="matlab",
                              cvx.modifiers=NULL, unique.string="") {
  ## Simple R interface to CVX. Allows a sequence of problems to be solved where
  ##   a single scalar parameter is varied, but otherwise the problems are
  ##   identical.
  ##
  ## Example: For lasso, tuning.param would be list(lam=c(0.1, 1, 2)).
  ##          const.vars would contain x and y as usual. This would solve the
  ##          lasso at the provided values of the tuning parameter lam.
  ##
  ## Arguments:
  ##   cvx.code: string containing call to CVX, i.e., what's inside cvx_begin
  ##             and cvx_end. Use the special word "tuningparam" for the
  ##             variable that will be iterated.
  ##   const.vars: list of non-optimization variables used in CVX expression.
  ##               labels of list elements should be the name of the variable.
  ##   tuning.param: a list with a single vector (with a name). E.g.,
  ##                 list(lam=c(0.1, 1, 2)). Each element of this vector is a
  ##                 level of the parameter. The problem will be solved at
  ##                 each such level.
  ##   opt.var.names: array of names of optimization variables.
  ##   setup.dir: directory containing the file cvx_setup.  If not needed,
  ##              leave null.
  ##   norun: doesn't call Matlab.  Returns the command that would be run
  ##          in Matlab.
  ##   matlab.call: how Matlab can be invoked through "system" command.
  ##                Default: "matlab" but even if this is the alias in
  ##                your default shell, "system" might use a different shell
  ##                in which "matlab" is not recognized.
  ##   cvx.modifiers: optional string of modifiers passed to CVX on same line
  ##                  as cvx_begin. E.g., "quiet" or "sdp".
  ##   unique.string: a character string added to temporary files. This is
  ##                  necessary if running in parallel (for example with
  ##                  parallel::mclapply) since the base::tempfile function
  ##                  may not produce unique file names otherwise.
  ##                  See help(tempfile).
  ##
  ## Returns:
  ##   cvx_optval (as returned by CVX), a sequence of optimal points found
  ##              by CVX, and the total time to solve all problems.
  setup <- ""
  if (!is.null(setup.dir)) {
    if (!("cvx_setup.m" %in% list.files(setup.dir)))
      stop("Could not find cvx_setup.m in user-provided \'setup.dir\'.")
    setup <- sprintf("dir = pwd; cd %s; cvx_setup; cd(dir);", setup.dir)
  }
  if (is.null(cvx.modifiers)) cvx.modifiers <- ""
  ## SEEMS TO GET CAUGHT WHEN " * " IS USED.  SO REMOVE SPACES IN THIS CASE
  cvx.code <- gsub(" +[*] +", "*", cvx.code)
  special <- c("time", "iter", "out")
  if (any(special %in% opt.var.names))
    stop("'time' and 'iter' are invalid names.")
  if (class(tuning.param) != "list" || length(tuning.param) != 1)
    stop("tuning.param must be a list with a single element.")
  if (class(tuning.param[[1]]) != "numeric")
    stop("tuning.param[[1]] must be a numeric vector.")
  if (is.null(names(tuning.param)))
    stop("names(tuning.param) must be nonnull.")
  param.name <- names(tuning.param)
  const.vars[[sprintf("%s_all", param.name)]] <- tuning.param[[1]]
  nprob <- length(tuning.param[[1]])
  after <- ""
  opt.var.names <- c(opt.var.names, "cvx_optval")
  for (out in opt.var.names) {
    ## RENAME EACH OUT VARIABLE: MYVAR_ITER = MYVAR
    ## (WHERE EVAL IN MATLAB WILL REPLACE ITER BY ITS VALUE)
    after <- sprintf("%s eval(sprintf('%s_%s=%s;', iter));",
                     after, out, "%d", out)
  }
  code <- sprintf(paste0("for iter=1:%s; %s=%s_all(iter); ",
                         "cvx_begin %s; %s; cvx_end; %s end"),
                  nprob, param.name, param.name,
                  cvx.modifiers, cvx.code, after)
  matlab.code <- sprintf(paste0("%s tStart=tic; %s; time=toc(tStart);",
                                "disp(['time elapsed: ',num2str(time)])"),
                         setup, code)
  output.names <- NULL
  for (out in opt.var.names) {
    output.names <- c(output.names, sprintf("%s_%s", out, seq(nprob)))
  }
  cvx <- CallMatlab(matlab.code, inputs = const.vars,
                    output.names = c(output.names, "time"),
                    norun = norun, matlab.call = matlab.call,
                    unique.string = unique.string)
  if (norun) return(cvx)
  cvx2 <- list()
  for (out in opt.var.names) {
    cvx2[[out]] <- list()
    for (i in seq(nprob)) {
      cvx2[[out]][[i]] <- cvx[[sprintf("%s_%s", out, i)]]
    }
  }
  cvx2[[param.name]] <- tuning.param[[1]]
  cvx2$cvx_optval <- as.numeric(cvx2$cvx_optval)
  cvx2$time <- cvx$time
  cvx2$command <- cvx$command
  cvx2
}
