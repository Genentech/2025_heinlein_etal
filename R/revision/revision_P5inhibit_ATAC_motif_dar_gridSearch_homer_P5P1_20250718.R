#After DAR, look for motifs from the resutant DAR peaks 
# For the new data different treatments for P5 20250718
# pano vs p1

library(data.table)
library(dplyr)
library(magrittr)

Sys.setenv(PATH = paste("/gstore/apps/Perl/5.26.1-foss-2017a/bin:/gstore/apps/DB/6.2.32-foss-2017a/bin:/gstore/apps/FFTW/3.3.6-gompi-2017a/bin:/gstore/apps/OpenBLAS/0.2.19-GCC-6.3.0-2.28-LAPACK-3.7.0/bin:/gstore/apps/OpenMPI/2.1.0-GCC-6.3.0-2.28/bin:/gstore/apps/hwloc/1.11.6-GCC-6.3.0-2.28/sbin:/gstore/apps/hwloc/1.11.6-GCC-6.3.0-2.28/bin:/gstore/apps/numactl/2.0.11-GCCcore-6.3.0/bin:/gstore/apps/binutils/2.28-GCCcore-6.3.0/bin:/gstore/apps/GCCcore/6.3.0/bin:/gstore/apps/modulefiles/RP/bin:/gstore/apps/Anaconda3/5.0.1:/gstore/apps/Anaconda3/5.0.1/bin:/cm/shared/apps/slurm/current/sbin:/cm/shared/apps/slurm/current/bin:/gstore/apps/utils/bin:/usr/lib64/qt-3.3/bin:/usr/local/bin:/usr/bin:/usr/local/sbin:/usr/sbin:/opt/ibutils/bin:/gstore/home/lix335/bin:/gstore/home/lix335/.local/bin:/gstore/home/lix335/HOMER/bin", Sys.getenv("PATH"), sep = ":"))



dar = fread("/gstore/project/crc_metastasis/revision/P5_inhibit_ATAC/crc_afterNorm_tmm_report_p5_p1.tsv",
            stringsAsFactors = F, data.table = F)%>%
  dplyr::arrange(Fold)


dar_emp1 = dar%>%
  dplyr::filter(seqnames == "chr6", start > 135362000, start < 135372000)

main_dir = "/gstore/project/crc_metastasis/revision/P5_inhibit_ATAC/HOMER_result/P5_P1/"



bg_peaks = dar%>%
  dplyr::select(seqnames, start, end, strand)%>%
  dplyr::mutate(ID = paste(seqnames, start, end, sep = "_"))%>%
  dplyr::mutate(notused = "")%>%
  dplyr::select(seqnames, start, end, ID, notused, strand)

write.table(bg_peaks, paste0(main_dir , "bg_peak.bed"),
            quote = F, row.names = F, col.names = F, sep = "\t")


#### sig is -log10(FDR)

lfc_cutoffs = c(round(log2(1.5),2),  log2(2))
sig_cutoffs = seq(4,10,2)



for(i in 1:length(lfc_cutoffs))
{
  for(j in 1:length(sig_cutoffs))
  {
    
    
    
    lfc_c = lfc_cutoffs[i]
    sig_c = sig_cutoffs[j]
    
    
    
    
    sel_up = dar%>%
      dplyr::filter(Fold>lfc_c, -log10(FDR) > sig_c)%>%
      dplyr::select(seqnames, start, end, strand)%>%
      dplyr::mutate(ID = paste(seqnames, start, end, sep = "_"))%>%
      dplyr::mutate(notused = "")%>%
      dplyr::select(seqnames, start, end, ID, notused, strand)
    
    tag = paste0("r", lfc_cutoffs[i],"_",sig_cutoffs[j],"_up")
    
    write.table(sel_up, paste0(main_dir ,tag, "_up_peak.bed"),
                quote = F, row.names = F, col.names = F, sep = "\t")
    
    
    this_dir = paste0(main_dir, 
                      tag )
    
    dir.create(this_dir, showWarnings = FALSE)
    
    setwd(main_dir)
    
    cmd = paste0("findMotifsGenome.pl ",tag, "_up_peak.bed mm10 ",this_dir, " -bg bg_peak.bed -nomotif -size 200 -mask")
    
    system(cmd)
    
    
    sel_down = dar%>%
      dplyr::filter(Fold< -lfc_c, -log10(FDR) > sig_c)%>%
      dplyr::select(seqnames, start, end, strand)%>%
      dplyr::mutate(ID = paste(seqnames, start, end, sep = "_"))%>%
      dplyr::mutate(notused = "")%>%
      dplyr::select(seqnames, start, end, ID, notused, strand)
    
    tag = paste0("r", lfc_cutoffs[i],"_",sig_cutoffs[j],"_down")
    
    write.table(sel_down, paste0(main_dir ,tag, "_down_peak.bed"),
                quote = F, row.names = F, col.names = F, sep = "\t")
    
    this_dir = paste0(main_dir, 
                      tag )
    
    dir.create(this_dir, showWarnings = FALSE)
    
    setwd(main_dir)
    
    cmd = paste0("findMotifsGenome.pl ", tag,"_down_peak.bed mm10 ",this_dir, " -bg bg_peak.bed -nomotif -size 200 -mask")
    
    system(cmd)
    
    
    
    cat("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ", i,j)
    
    
    
    
  }
  
  
}

