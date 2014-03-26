## Mike is testing the synchronization on home computer
## with config in GIT/repomania/inst/extdata/raad_repo_config.json

## 2014-03-26
## "SMMR-SSM/I Nasateam daily sea ice concentration
## do_sync set to true, sync_from_year set to 2014


options()[c("default.datadir", "repo.config")]
$default.datadir
[1] "/etc/repo/data"

$repo.config
[1] "/etc/R/R/library/repomania/extdata/raad_repo_config.json"

 ic <- repomania:::.icefiles()

 head(ic$fullname)
[1] "E:/DATA/aad/repo/data/seaice/smmr_ssmi_nasateam/daily/1995/nt_19950101_f11_v01_s.bin"
[2] "E:/DATA/aad/repo/data/seaice/smmr_ssmi_nasateam/daily/1995/nt_19950101_f11_v01_s.bin"
[3] "E:/DATA/aad/repo/data/seaice/smmr_ssmi_nasateam/daily/1995/nt_19950102_f11_v01_s.bin"
[4] "E:/DATA/aad/repo/data/seaice/smmr_ssmi_nasateam/daily/1995/nt_19950102_f11_v01_s.bin"
[5] "E:/DATA/aad/repo/data/seaice/smmr_ssmi_nasateam/daily/1995/nt_19950103_f11_v01_s.bin"
[6] "E:/DATA/aad/repo/data/seaice/smmr_ssmi_nasateam/daily/1995/nt_19950103_f11_v01_s.bin"
> tail(ic$fullname)
[1] "E:/DATA/aad/repo/data/seaice/smmr_ssmi_nasateam/daily/2013/nt_20140111_f17_nrt_s.bin"
[2] "E:/DATA/aad/repo/data/seaice/smmr_ssmi_nasateam/daily/2013/nt_20140111_f17_nrt_s.bin"
[3] "E:/DATA/aad/repo/data/seaice/smmr_ssmi_nasateam/daily/2013/nt_20140112_f17_nrt_s.bin"
[4] "E:/DATA/aad/repo/data/seaice/smmr_ssmi_nasateam/daily/2013/nt_20140112_f17_nrt_s.bin"
[5] "E:/DATA/aad/repo/data/seaice/smmr_ssmi_nasateam/daily/2013/nt_20140113_f17_nrt_s.bin"
[6] "E:/DATA/aad/repo/data/seaice/smmr_ssmi_nasateam/daily/2013/nt_20140113_f17_nrt_s.bin"


## read config, with desired options
cf <- repo_config(getOption("repo.config"), getOption("default.datadir"))

