#' @title read.OUT
#'
#' @description This function allows to read the gmacsall.OUT to find estimates
#' of specific management/biological quantities
#'
#' Need to be modified to allow a vector of character to search any variable
#'
#' @param file Name (with directory) of the gmacsall.out
#'
#' @return list of management/biological quantities
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' }
#' 
read.OUT <- function(file){
  D <-  read.table(file,comment.char = "#",fill=T,blank.lines.skip=T,stringsAsFactors=F,col.names=1:100)
  Out <- NULL
  
  MMB <- MatchTable(D,Char1="ssb"); MMB <- as.numeric(D[MMB+1,])
  Out$MMB <- MMB[which(is.na(MMB))[1]-1]
  spr_bmsy <- MatchTable(D,Char1="spr_bmsy"); spr_bmsy <- as.numeric(D[spr_bmsy+1,])
  Out$B35 <- spr_bmsy[!is.na(spr_bmsy)]
  sd_fmsy <- MatchTable(D,Char1="sd_fmsy"); sd_fmsy <- as.numeric(D[sd_fmsy+1,])
  Out$F35 <- sd_fmsy[!is.na(sd_fmsy)][1]
  sd_fofl <- MatchTable(D,Char1="sd_fofl"); sd_fofl <- as.numeric(D[sd_fofl+1,])
  Out$FOFL <- sd_fofl[!is.na(sd_fofl)][1]
  spr_cofl <- MatchTable(D,Char1="spr_cofl"); spr_cofl <- as.numeric(D[spr_cofl+1,])
  Out$OFL <- spr_cofl[!is.na(spr_cofl)]
  Nat.M <- MatchTable(D,Char1="M"); Nat.M <- as.numeric(D[Nat.M+1,])
  Out$M <- mean(Nat.M[!is.na(Nat.M)])
  Out$Status <- Out$MMB/Out$B35
  
  recruits <- MatchTable(D,Char1="recruits"); recruits <- as.numeric(D[recruits+1,])
  Out$recruits <- recruits[!is.na(recruits)]
  Out$Av_Recr <- mean(Out$recruits)/10000
  return(Out)
}