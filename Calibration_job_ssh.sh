#PBS -l walltime=12:00:00
#PBS -l select=1:ncpus=4:mem=20gb

module load anaconda3/personal
source activate R-env

cd /rds/general/user/md2620/home/asthma/Malo/R_Scripts

Rscript Calibration_job.R