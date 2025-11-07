
library(data.table)
library(dplyr)
library(magrittr)

Sys.setenv(PATH = paste("/gstore/apps/Perl/5.26.1-foss-2017a/bin:/gstore/apps/DB/6.2.32-foss-2017a/bin:/gstore/apps/FFTW/3.3.6-gompi-2017a/bin:/gstore/apps/OpenBLAS/0.2.19-GCC-6.3.0-2.28-LAPACK-3.7.0/bin:/gstore/apps/OpenMPI/2.1.0-GCC-6.3.0-2.28/bin:/gstore/apps/hwloc/1.11.6-GCC-6.3.0-2.28/sbin:/gstore/apps/hwloc/1.11.6-GCC-6.3.0-2.28/bin:/gstore/apps/numactl/2.0.11-GCCcore-6.3.0/bin:/gstore/apps/binutils/2.28-GCCcore-6.3.0/bin:/gstore/apps/GCCcore/6.3.0/bin:/gstore/apps/modulefiles/RP/bin:/gstore/apps/Anaconda3/5.0.1:/gstore/apps/Anaconda3/5.0.1/bin:/cm/shared/apps/slurm/current/sbin:/cm/shared/apps/slurm/current/bin:/gstore/apps/utils/bin:/usr/lib64/qt-3.3/bin:/usr/local/bin:/usr/bin:/usr/local/sbin:/usr/sbin:/opt/ibutils/bin:/gstore/home/lix335/bin:/gstore/home/lix335/.local/bin:/gstore/home/lix335/HOMER/bin", Sys.getenv("PATH"), sep = ":"))



main_dir = "/gstore/project/crc_metastasis/revision/P5_inhibit_ATAC/PCA_merge/HOMER_result_5000/"



pcs = c("pc1","pc2","pc3")
topN =5000


for(i in 1:length(topN))
{
  for(j in 1:length(pcs))
  {
    
    
    file_name_up = paste0("/gstore/project/crc_metastasis/revision/P5_inhibit_ATAC/PCA_merge/", pcs[j], "_",topN[i], "_up_peak.bed")
    file_name_down = paste0("/gstore/project/crc_metastasis/revision/P5_inhibit_ATAC/PCA_merge/", pcs[j], "_",topN[i], "_down_peak.bed")
    file_name_bg = paste0("/gstore/project/crc_metastasis/revision/P5_inhibit_ATAC/PCA_merge/", pcs[j],  "_bg_peak.bed")
    
    tag = paste0(pcs[j], "_", topN[i], "_up")
    
    this_dir = paste0(main_dir, 
                      tag )
    
    dir.create(this_dir, showWarnings = FALSE)
    
    setwd(main_dir)
    
    cmd = paste0("findMotifsGenome.pl ",file_name_up,  " mm10 ", this_dir, " -bg ", file_name_bg, " -nomotif -size 200 -mask -p 4")
    
    system(cmd)
    
    
    
    tag = paste0(pcs[j], "_", topN[i], "_down")
    
    this_dir = paste0(main_dir, 
                      tag )
    
    dir.create(this_dir, showWarnings = FALSE)
    
    setwd(main_dir)
    
    cmd = paste0("findMotifsGenome.pl ",file_name_down,  " mm10 ", this_dir, " -bg ", file_name_bg, " -nomotif -size 200 -mask -p 4")
    
    system(cmd)
    
    
    cat("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ", i,j)
    
    
    
    
  }
  
  
}