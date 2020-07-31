  library(ncdf4)
  library(fields)
  library(stringr)
  
  CDO_PR_INTERPOLATE<-function(first_year_of_extraction,last_year_of_extraction,# range_file, #last_date_of_extraction,
                                model=Model,
                                model_version=Model_version,
                                run=Run,
                                freq=Freq,
                                grid_output=Grid_1x1){
    load('models_list.rdata')
    model_5mod <- TAS_MODELS[c(2,3,4,5,7,8,11,12,13,16,17,20,22,24,26,30,31,33,34,37),1]  ### Modèles utilisés (dans l'ordre) : CNRM,IPSL,MPI,GISS,NORESM
    model_version_5mod <- TAS_MODELS[c(2,3,4,5,7,8,11,12,13,16,17,20,22,24,26,30,31,33,34,37),2]
    for(i in 1:length(model)){
      var="pr"
      Grid_1x1 = paste("/home/mgarvik/extract_files/my_grid.txt")
      
      # exercise_input=exercise
      exercise_output="hist_rcp"
      
      path_input_hist=paste("/bdd/CMIP5/output",model[i],model_version[i],"historical/mon/atmos/Amon/r1i1p1/latest/pr/",sep="/")
      path_input_rcp=paste("/bdd/CMIP5/output",model[i],model_version[i],"rcp85/mon/atmos/Amon/r1i1p1/latest/pr/",sep="/")
      path_output=paste("/data/mgarvik/CMIP5_30Ayr/pr",model[i],sep="/")
      # path_output_complete=paste("/data/mgarvik/CMIP5_Ayr/pr",model[i],sep="/")
      name_file=paste(var,freq,model_version[i],exercise_output,run,sep="_")
      # name_file_complete=paste(var,"Ayr",model_version[i],exercise_output,run,sep="_")
      List_name_file_hist= list.files(path_input_hist)
      List_name_file_rcp= list.files(path_input_rcp)  
      first_year_in_data=as.numeric(str_sub(List_name_file_hist[1],-16,-13))
      
      #### How much files do we need to read to cover the whole desired time series btw first_year_of_extraction and last_year_of_extraction?
      list_last_year_file_hist=as.numeric(str_sub(List_name_file_hist,-9,-6))
      list_last_year_file_rcp=as.numeric(str_sub(List_name_file_rcp,-9,-6))
      #pour le max, on prend le premier file ayant une date de fin au moins plus grande 
      #que la date correspondante a first_year_in_data + etendue de temps desire
      range_file_max=(which(list_last_year_file_hist>=last_year_of_extraction))[1]
      
      range_file=1:range_file_max
      ###CDO command to interpolate to the grid specified and delete feb29
      cmd_cdo=c()
      
      for(file in range_file){
        cmd_cdo=paste0(cmd_cdo,paste0("-remapbil,",grid_output," ",path_input_hist, List_name_file_hist[file]," "))
        cmd_cdo=paste0(cmd_cdo,paste0("-remapbil,",grid_output," ",path_input_rcp, List_name_file_rcp[file]," "))
      }
      tmp_first_date_of_extraction=str_sub(List_name_file_hist[min(range_file)],-16,-11)
      # tmp_last_date_of_extraction=str_sub(List_name_file_rcp[max(range_file)],-9,-4)
      first_date_of_extraction=as.character(paste0(first_year_of_extraction,"01"))
      last_date_of_extraction=as.character(paste0(last_year_of_extraction,"12"))
      
      system(paste0("mkdir ",path_output,"/tmp_netcdf",sep=""))
      
      #merge all the files together
      system(paste0("cdo mergetime ",
                    cmd_cdo,
                    paste0(path_output,"/tmp_netcdf/"),
                    paste0("tmp_",name_file,"_",first_date_of_extraction,"_",last_date_of_extraction,".nc")))
      
      #Cut the years and stored in path_output
      system(paste0("cdo -selyear,", first_year_of_extraction,"/",last_year_of_extraction," ",
                    paste0(path_output,"/tmp_netcdf/"),
                    paste0("tmp_",name_file,"_",first_date_of_extraction,"_",last_date_of_extraction,".nc "),
                    paste0(path_output,"/tmp_netcdf/",
                    "tmp2_",name_file,"_",first_date_of_extraction,"_",last_date_of_extraction,".nc")))
      
      #Amon to Ayr
      system(paste0("cdo -ymonmean,", first_year_of_extraction,"/",last_year_of_extraction," ",
                    paste0(path_output,"/tmp_netcdf/"),
                    paste0("tmp2_",name_file,"_",first_date_of_extraction,"_",last_date_of_extraction,".nc "),
                    paste0(path_output,"/tmp_netcdf/",
                    "tmp3_",name_file,"_",first_date_of_extraction,"_",last_date_of_extraction,".nc")))
      
      # #Ayr to 30Ayr
      system(paste0("cdo -timmean,", " ",
                    paste0(path_output,"/tmp_netcdf/"),
                    paste0("tmp3_",name_file,"_",first_date_of_extraction,"_",last_date_of_extraction,".nc "),
                    path_output,"/",
                    name_file,"_",first_date_of_extraction,"_",last_date_of_extraction,".nc"))
      
      # # Change unit from kg.m-2.s-1 to mm/day
      # system(paste0("cdo -mulc,86400 ",
      #               paste0(path_output,"/tmp_netcdf/"),
      #               paste0("tmp_",name_file,"_",first_date_of_extraction,"_",last_date_of_extraction,".nc "),
      #               path_output,"/",
      #               name_file,"_",first_date_of_extraction,"_",last_date_of_extraction,".nc"))
      # 
      # system(paste0("cdo -setunit,'mm/day' ",
      #               paste0(path_output,"/tmp_netcdf/"),
      #               paste0("tmp_",name_file,"_",first_date_of_extraction,"_",last_date_of_extraction,".nc "),
      #               path_output,"/",
      #               name_file,"_",first_date_of_extraction,"_",last_date_of_extraction,".nc"))
      
      
      system(paste0("rm -rf /data/mgarvik/CMIP5_30Ayr/pr/",model[i],"/tmp_netcdf",sep=""))
    }
  }