#' @include classes.R

#' @description This generic function is used to submit a Rscript job to a Slurm cluster. Current support is for Singularity and Conda environments.
#'
#'
#' @title Submit: Submit an R script to a Slurm cluster
#' @param x A \linkS4class{Submission} class object, or a string containing a name for a new job submission.
#' @param ... Other arguments
#' @details It is suggested that you use clean.remote() after your job has completed to clean the remote
#' address of the .slurmSubmit/ hidden directory.
#' @seealso \linkS4class{Submission}, \linkS4class{JobProfile}, \link{clean.remote}
#' @examples
#' # Naive method
#' submit("example", cluster="login.sumner.jax.org", slurm.args="-q batch")
#'
#' # Without any job profile specified, the default JobProfile object will be used.
#'
#' # The preferred method is to define a Submission type object to pass to submit()
#'
#' exampleProfile = JobProfile(cpus=2, memory="4G", qos="batch",email.complete="matt.bradley@@jax.org")
#' exampleSub = Submission(script.file="example.R", cluster="login.sumner.jax.org", profile=exampleProfile)
#'
#' submit(exampleSub, job.name="example")
#'
#' #This will save a Submission type object named "example" after completing successfully.
#'
#' # Alternatively, submit.default will create a new Submission object using passed parameters. It is important
#' # to note that job.name is required to trigger the default method. In this example we will also define
#' # sbatch arguments using the slurm.args argument. Since save.sub=TRUE, this will create a new Submission
#' # type object in .GlobalEnv named "example".
#'
#' submit("example", script.file="example.R", cluster="login.sumner.jax.org",
#'        slurm.args="-c 2 --mem=4G, -q batch --mail-type=END --mail-user=matt.bradley@@jax.org",
#'        save.sub=TRUE)
#'
#' @rdname submit
#' @export submit
submit <- function(x, ...) UseMethod("submit", x)

#' @rdname submit
#' @param job.name Name of job to be submitted. REQUIRED for default method.
#' @param script.file Relative path to the script
#' @param cluster Address of cluster you want to submit to.
#' @param slurm.args String containing all \code{sbatch} arguments for your submission. Takes precedence over values in the
#' \linkS4class{JobProfile}  passed to the \code{profile} argument.
#' @param profile A \linkS4class{JobProfile} type object.
#' @param script.args String containing all arguments to the submitted Rscript to be executed remotely on the cluster.
#' @param conda String containing the name of a valid conda environment on the cluster to be used to execute R.
#' @param singularity String path or URI of a Singularity container to execute Rscript inside of.
#' @param r.bin.args String containing all arguments to pass to the \code{RScript} binary.
#' @param r.data A character vector containing the name(s) of valid R data objects in the search path. This data will be made
#' available locally on the cluster in the same directory as the script under the file handle \code{Rdata}.
#' @param local.data String containing a list of paths to one or more local file(s). WARNING: Large files may take a long time to sync.
#' @param target.dir Path to directory to create .slurmsubmit/ directory.
#' @param VERBOSE Show more details.
#' @param save.sub If \code{TRUE} will save this job submission as a Submission type object under \code{job.name}.
#' @param save.profile.name If specified, will save the generated profile object under the specified name in .Globalenv.
#' @method submit default
#' @export
submit.default <- function(job.name,script.file="", cluster="", slurm.args="", script.args="",
                           conda="", singularity="docker://rocker/r-base:latest", r.bin.args="",
                           target.dir="~", r.data="", local.data="", profile=JobProfile(), save.profile.name="",
                           VERBOSE=FALSE, save.sub=FALSE){
  if(missing(profile)) profile<-JobProfile(other.args=slurm.args)
  if(!missing(save.profile.name)){
    cat(paste0("Creating job profile ", save.profile.name), fill=TRUE)
    assign(save.profile.name, profile,envir=.GlobalEnv)
  }
  submit.Submission(sub=Submission(script.file=script.file, script.args=script.args, cluster=cluster,
                               profile=profile, singularity=singularity, conda=conda, r.bin.args=r.bin.args,
                               target.dir=target.dir, r.data=r.data, local.data=local.data), VERBOSE=VERBOSE,
                               job.name=job.name,save.sub=save.sub)
}
#' @rdname submit
#' @param sub A Submission type object
#' @param save.sub If TRUE will save this job submission as a \linkS4class{Submission} type object under \code{job.name}.
#' @method submit Submission
#' @export
submit.Submission = function(sub,job.name="R-slurmSubmit",save.sub=FALSE, VERBOSE=FALSE){
  if(sub@script.file==""){
    if(VERBOSE){cat("No `script.file` specified. Prompting for file.", fill=TRUE)}
    sub@script.file=file.choose()
  }
  setwd("~")
  if(length(sub@local.data)>0){
    if(VERBOSE){cat("Copying local files...", fill=TRUE)}
    file.copy(sub@local.data, ".slurmSubmit/", recursive =TRUE, overwrite=TRUE )
  } else {
    dir.create(".slurmSubmit")
  }
  if(length(sub@r.data)>0){
    if(VERBOSE){cat("Copying R data objects...", fill=TRUE)}
    save(list=sub@r.data, file=".slurmSubmit/Rdata")
  }
  if(VERBOSE){cat("Preparing local directory...", fill=TRUE)}
  file.copy(sub@script.file,".slurmSubmit/")
  script = basename(sub@script.file)
  rscript.call = paste(c("Rscript", sub@r.bin.args, paste0(sub@target.dir,"/.slurmSubmit/",script), sub@script.args))
  if(!exists("sub@conda")){
    submit.script = cat(paste(c("#!/bin/bash\nmodule load singularity\n","singularity exec",
                                sub@singularity,rscript.call)),file=".slurmSubmit/submit.sh")
  } else {
    submit.script = cat(paste(c("#!/bin/bash\nconda activate ",sub@conda,"\n"
                                ,rscript.call)),file=".slurmSubmit/submit.sh")
  }
  if(VERBOSE){cat("Script created successfully... syncing to cluster", fill=TRUE)}
  cat(paste0("Syncing files to ", sub@cluster), fill=TRUE)
  if(VERBOSE){cat(paste0("Creating ", sub@target.dir, "/.slurmSubmit/ on ", sub@cluster), fill=TRUE)}
  if(VERBOSE){cat(paste0("rsync -aP .slurmSubmit/ ", sub@cluster,":",sub@target.dir,"/.slurmSubmit/"), fill=TRUE)}
  sys::exec_wait("rsync",c("-aP", ".slurmSubmit/", paste0(sub@cluster,":",sub@target.dir,"/.slurmSubmit")))
  cat("Files successfully synced. Submitting job to cluster...", fill=TRUE)
  submit.command = paste(c("sbatch",
                           "--mem" , sub@profile@memory,
                           "-c", sub@profile@cpus,
                           "-t", sub@profile@walltime,
                           if(length(sub@profile@qos)>0) paste("-q",sub@profile@qos),
                           "-o", sub@profile@output.file,
                           "-J", job.name,
                           if(length(sub@profile@email.complete)>0) paste0("--mail-type=END --mail-user=",sub@profile@email.complete),
                           if(length(sub@profile@other.args)>0) paste(sub@profile@other.args),
                           paste0(sub@target.dir,"/.slurmSubmit/submit.sh")), sep = " ")
  if(VERBOSE){cat("Slurm command used:", fill=TRUE)}
  if(VERBOSE){cat(submit.command, fill=TRUE)}
  sys::exec_wait("ssh", c(sub@cluster, submit.command))
  unlink(".slurmSubmit", recursive=TRUE,force=TRUE)

  if(save.sub){
    cat(paste0("Creating submission profile ", job.name), fill=TRUE)
    assign(job.name, sub, envir=.GlobalEnv)
  }
}

#' @title Clean slurmSubmit directory
#'
#' @description This function is a helper function to delete the \code{.slurmSubmit} directory created when the
#' \code{\link{submit}} is run.
#'
#' @param target.dir Path to the parent directory of .slurmsubmit/
#' @param address Address of remote to clean
#' @param sub \linkS4class{Submission} type object containing remote address.
#' @details Argument \code{address} takes precedence over \code{sub} if specified.
#' @seealso \code{\link{submit}}
#'
#' @export
clean.remote = function(address="", target.dir = "~/", sub=Submission()){
  if(missing(address)) address <- sub@cluster
  return(sys::exec_wait("ssh", c(address, paste0("rm -rf ",target.dir,"/.slurmSubmit/"))))
}

