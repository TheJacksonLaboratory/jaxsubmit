# Legacy - Used to check JobProfile check.
# check_profile <- function(object){
#   errors <- character()
#   if(object@cpus>70){
#     errors <- c(errors,paste0("cpus is ", object@cpus, ". Should be 70 or fewer."))
#   }
#   if(length(errors)==0) TRUE else errors
# }

## Check Submission object constructor validity
# check_submission <- function(object){
#   errors <- character()
#   if(length(object@r.data) > 0){
#     for(i in object@r.data){
#       if(!exists(i)) errors <- c(errors,paste0("r.data argument ", i," does not exist."))
#     }
#   }
#   if(length(object@local.data) > 0){
#     for(i in object@local.data){
#       if(!file.exists(i)) errors <- c(errors,paste0("local.data argument", i, " file does not exist"))
#     }
#   }
#   if(length(errors) == 0) TRUE else errors
# }
#' @title JobProfile constructor
#'
#' @description An S4 class used to create a job specification.
#'
#' @slot memory The amount of memory used by the job
#' @slot cpus The number of cpus used by the job
#' @slot walltime The maximum amount of time the job should run
#' @slot qos Name of the "quality of service" (qos) that the job should be submitted as
#' @slot output.file The path to the Slurm output file
#' @slot email.complete If used, Slurm will email the given email address when the job is finished.
#' @slot other.args A string containing additional flags and values to pass to Slurm. These will overwrite previously given parameters when submitted.
#' @seealso \code{\linkS4class{Submission}}, \code{\link{submit}}
#' @export JobProfile
JobProfile = setClass("JobProfile", representation(memory="character", cpus="numeric",
                      walltime="character", qos="character", output.file="character", email.complete = "character",
                      other.args="character"), prototype(memory="1G", cpus=1, walltime="4:00:00",
                                                         output.file=".slurmSubmit/%j.out"))

#' @title Submission constructor
#' @description An S4 class used to create a job submission.
#'
#' @slot script.file Relative path to the script
#' @slot cluster Address of cluster you want to submit to.
#' @slot profile A \linkS4class{JobProfile} type object.
#' @slot script.args String containing all arguments to the submitted Rscript to be executed remotely on the cluster.
#' @slot conda String containing the name of a valid conda environment on the cluster to be used to execute R.
#' @slot singularity String path or URI of a Singularity container to execute Rscript inside of.
#' @slot r.bin.args String containing all arguments to pass to the `Rscript` binary.
#' @slot r.data String containing the name of an R data object in the search path.
#' @slot local.data String containing a list of paths to one or more local file(s). WARNING: Large files may take a long time to sync.
#' @slot target.dir Path to directory to create .slurmsubmit/ directory.
#' @seealso \code{\linkS4class{JobProfile}}, \code{\link{submit}}
#' @export Submission
Submission = setClass("Submission", representation(script.file="character", script.args="character",
                      cluster="character", profile="JobProfile", singularity="character", conda="character",
                      r.bin.args="character", r.data = "character",local.data="character", target.dir="character"),
                      prototype(cluster="login.sumner.jax.org", profile=JobProfile(),
                                singularity="docker://rocker/r-base:latest", target.dir="~"))


