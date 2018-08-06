
# Run SMS Analysis --------------------------------------------------------

source('R/fx_sms_import.R')
source('R/fx_sms_prepare.R')
source('R/fx_sms_visualise.R')


fx_sms_import(xml_path = "C:/Users/exp01754/Downloads/sms-2018-08-06 14-21-13.xml",
              prior_master_date = "2018-05-31")

fx_sms_prepare(master_date = "2018-08-06", use_anon = TRUE)

fx_sms_visualise(master_date = "2018-08-06")
