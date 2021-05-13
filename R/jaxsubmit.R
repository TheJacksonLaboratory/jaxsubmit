
#' Submit a new Rscript to the cluster
#'
#' This function is used to submit a new Rscript job to the cluster. It will fill in a
#' cookie-cutter Slurm submission script and execute R either inside a Singularity container
#' or with an existing conda environment.
#'
#' @param script Relative path to the script
#' @param cluster Name of cluster you want to submit to (sumner or winter).
#' @param slurm.args String containing all `sbatch` arguments for your submission.
#' @param rscript.args String containing all arguments to the submitted Rscript to be executed remotely on the cluster.
#' @param conda String containing the name of a valid conda environment on the cluster to be used to execute R.
#' @param container String path or URI of a Singularity container to execute Rscript inside of.
#' @param r.bin.args String containing all arguments to pass to the `Rscript` binary.
#' @param target.dir Path to tier1 directory to create .jaxsubmit/ directory.
#'
#' @return Slurm job ID of submitted script
#' @export
#'
#' @examples submit(script="example.R", slurm.args="-q batch -t 4:00:00 -o '.jaxsubmit/%j.output'"
#' container="docker://rocker/tidyverse:latest", r.bin.args="--save")
submit = function( script="", cluster="sumner", slurm.args="-q batch", rscript.args="",
                   conda="", container="docker://rocker/r-base:latest", r.bin.args="",
                   target.dir="~"){
  setwd(target.dir)
  dir.create(".jaxsubmit")
  if(script==""){
    script=file.choose()
  }
  file.copy(script,".jaxsubmit/")
  script = basename(script)
  rscript.call = paste(c("Rscript", r.bin.args, paste0(target.dir,"/.jaxsubmit/",script), rscript.args))
  if(conda==""){
    submit.script = cat(paste(c("#!/bin/bash\nmodule load singularity\n","singularity exec",
                                container,rscript.call)),file=".jaxsubmit/submit.sh")
  } else {
    submit.script = cat(paste(c("#!/bin/bash\nconda activate ",conda,"\n"
                                ,rscript.call)),file=".jaxsubmit/submit.sh")
  }

  sys::exec_wait("rsync",c("-a", "./.jaxsubmit/", paste0("login.",cluster,".jax.org",":",target.dir,"/.jaxsubmit/")))
  submit.command = paste(c("sbatch", slurm.args, paste0(target.dir,"/.jaxsubmit/submit.sh")), sep = " ")
  sys::exec_wait("ssh", c("login.sumner.jax.org", submit.command))
  unlink(".jaxsubmit", recursive=TRUE,force=TRUE)
}

#' Clean .jaxsubmit directory
#'
#' This function is a helper function to delete the .jaxsubmit directory created when the
#' jaxsubmit::submit() command is run.
#'
#' @param target.dir Path to the parent directory of .jaxsubmit/
#'
#' @export
clean.remote = function(target.dir = "~/"){
  return(sys::exec_wait("ssh", c("login.sumner.jax.org", paste0("rm -rf ",target.dir,"/.jaxsubmit/"))))
}
