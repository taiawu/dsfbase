
# reads the dye screens from a named list of directories
# ... arguments get passed to dye_screen_to_dsfbase
add_all_dye_screens <- function(dye_screen_dirs, .save_path, ...){ # .min_range = .min_range, .save_data = .save_data, 
  
# # logs #aggregating dye screens
# dye_screen_dirs <- read_rds("/Users/taiaseanwu/Desktop/programming/dsfbase/00_raw_inputs/dye_screen_directories.rds")
# # "From Exp1252"

# HSP72 SBD
dye_screen_to_dsfbase(drop_full_screen = TRUE,
                      dye_screen_dirs["Ds00010--Hsp72_SBD"],
                    save_path = .save_path, # min_range = .min_range,
                    # save_data = .save_data, 
                    ...)

#p300_KIX: made some modifications and re-saved
dye_screen_to_dsfbase(dye_screen_dirs["Ds0007_p300_KIX"],
                   save_path = .save_path, # min_range = .min_range,
                    # save_data = .save_data,
                    drop_from_canon = c("H15_FAM_protein", "H3_JOE_protein",  "I3_JOE_protein"),
                    add_to_noncanon = c("H3_JOE_protein"),
                    drop_from_noncanon = c("A23_TAMRA_protein", "A24_TAMRA_protein", "A7_FAM_protein", "A8_Cy5_protein", "C11_Cy5_protein", "C14_TAMRA_protein", "C3_FAM_protein", "E11_Cy5_protein", "E17_Cy5_protein", "E8_FAM_protein",  "H2_FAM_protein", "I4_Cy5_protein", "I8_FAM_protein", "K12_FAM_protein", "K13_FAM_protein", "K14_FAM_protein", "K15_TAMRA_protein", "K17_TAMRA_protein", "K8_FAM_protein"),
                    drop_from_SYPRO_canon = c("E5_TAMRA_protein"),
                    add_to_gotcha = c("H15_FAM_protein", "I3_JOE_protein", "I4_Cy5_protein"), 
                    ...)

#PPIG : everything was a weird judgement call. Deleted the whole screen.
dye_screen_to_dsfbase(drop_full_screen = TRUE,
                    dye_screen_dirs["Ds0019_PPIG"],
                   save_path = .save_path, # min_range = .min_range,
                    # save_data = .save_data, 
                    ...)

#PWD1: no objections
dye_screen_to_dsfbase(   dye_screen_dirs["Ds0020_PPWD1"],
                   save_path = .save_path, # min_range = .min_range,
                    # save_data = .save_data, 
                    ...)

#Ds0021_PPIL1: no objections
dye_screen_to_dsfbase(   dye_screen_dirs["Ds0021_PPIL1"],
                   save_path = .save_path, # min_range = .min_range,
                    # save_data = .save_data, 
                    ...)

#Nurr1_LBD: made some modifications
dye_screen_to_dsfbase(dye_screen_dirs["Ds0029_Nurr1_LBD"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   
   add_to_canon = c("A15_Cy5_protein", "A4_TAMRA_protein", "C15_FAM_protein", "E5_JOE_protein"),
   drop_from_canon = c("A23_JOE_protein", "A23_ROX_protein",    "C14_TAMRA_protein",    "C3_FAM_protein"),
   add_to_noncanon = c("A23_JOE_protein", "A23_ROX_protein",    "C14_TAMRA_protein",    "C3_FAM_protein"),
   drop_from_noncanon = c("A19_Cy5_protein",    "A24_TAMRA_protein",       "E17_Cy5_protein", "H20_FAM_protein", "I16_JOE_protein", "I4_Cy5_protein",    "K20_Cy5_protein", "I8_FAM_protein"),
   add_to_gotcha = c("K20_Cy5_protein"),
   ...)

#CBP_KIX
dye_screen_to_dsfbase(dye_screen_dirs["Ds0030_CBP_KIX"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
  drop_from_canon = c("G15_ROX_protein", "G15_JOE_protein"),
  add_to_gotcha = c("G15_JOE_protein"),
  drop_from_noncanon = c( "A15_Cy5_protein", "A23_JOE_protein", "A24_TAMRA_protein", "A4_TAMRA_protein", "A6_ROX_protein", "A8_Cy5_protein", "C11_Cy5_protein", "C13_JOE_protein", "C14_TAMRA_protein", "C3_FAM_protein", "C4_TAMRA_protein", "C7_TAMRA_protein", "E11_Cy5_protein",    "E17_Cy5_protein", "E20_Cy5_protein", "E3_ROX_protein",    "I3_JOE_protein", "I4_Cy5_protein", "K21_ROX_protein"),
  ...)

#GB1
dye_screen_to_dsfbase(dye_screen_dirs["Ds0031_GB1"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   
  drop_from_canon = c("I3_JOE_protein"),
  add_to_gotcha = c("I3_JOE_protein", "H7_FAM_protein"),
  drop_from_noncanon = c("A12_Cy5_protein", "A15_Cy5_protein", "A19_Cy5_protein", "C14_TAMRA_protein", "C2_JOE_protein", "C4_TAMRA_protein", "C7_TAMRA_protein", "E17_Cy5_protein", "E20_Cy5_protein", "E3_ROX_protein", "H2_FAM_protein", "H7_FAM_protein", "H8_FAM_protein",    "K19_TAMRA_protein" ),
  ...)

#Exp0468_eIF2B_no_ISIB -#added none, since this sample was probably contaminated with ISRIB
dye_screen_to_dsfbase(drop_full_screen = TRUE,
                       dye_screen_dirs["Exp0468--eIF2B_no_ISIB"],
                   save_path = .save_path, # min_range = .min_range,
                    # save_data = .save_data, 
                    ...)

#Exp0469_eIF2B_with_ISRIB #no changes
dye_screen_to_dsfbase(dye_screen_dirs["Exp0469--eIF2B_with_ISRIB"],
                   save_path = .save_path, # min_range = .min_range,
                    # save_data = .save_data, 
                    ...)

#Exp0505_kHvL_light_chain
dye_screen_to_dsfbase(dye_screen_dirs["Exp0505--kHvL_light_chain"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   
   drop_from_noncanon = c("A10_TAMRA_protein_top_plate", "E23_Cy5.5_protein_bottom_plate", "E24_Cy5.5_protein_bottom_plate"), 
   ...)

#Exp0506_Bag2
dye_screen_to_dsfbase(dye_screen_dirs["Exp0506--Bag2"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   
   drop_from_canon = c("C1_FAM_protein_top_plate"),
   add_to_noncanon = c("C1_FAM_protein_top_plate"), 
   ...)

#Hsc70
dye_screen_to_dsfbase(dye_screen_dirs["Exp0507--Hsc70"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   
   drop_from_canon = c("E14_FAM_protein_MWA021"),
   add_to_noncanon = c("E14_FAM_protein_MWA021"), 
   ...)

#HIP
dye_screen_to_dsfbase(dye_screen_dirs["Exp0508--HIP"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   
   drop_from_noncanon = c( "C1_FAM_protein_top_plate", "E1_TAMRA_protein_bottom_plate",    "E3_TAMRA_protein_bottom_plate",    "E5_TAMRA_protein_bottom_plate", "E6_TAMRA_protein_bottom_plate", "E8_TAMRA_protein_bottom_plate"),
   ...)

#KaiC AE
dye_screen_to_dsfbase(dye_screen_dirs["Exp0584_KaiC_AE"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   
   drop_from_canon = c("G9_FAM_protein", "J4_FAM_protein", "K5_Cy5_protein", "L10_TAMRA_protein"),
   add_to_noncanon = c("G9_FAM_protein","J4_FAM_protein","K5_Cy5_protein"),
   drop_from_noncanon = c("D8_TAMRA_protein"),
   ...)

#Kai EA
dye_screen_to_dsfbase(dye_screen_dirs["Exp0584_KaiC_EA"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_canon = c("D8_ROX_protein", "D8_TAMRA_protein"),
   ...)
# 
#    add_to_noncanon = c(),
#    save_path = "../02_aggregated_data/dye_screen_tests/")

#Exp0595_ACD_domain_protein6_canon #no. modifications
dye_screen_to_dsfbase(  dye_screen_dirs["Exp0595--ACD_domain_protein6"],
                   save_path = .save_path, # min_range = .min_range,
                    # save_data = .save_data, 
                    ...)

#Exp0596_H104-FL-B5-pre-inc_protein5 #all trash / not worth it. don't keep any
dye_screen_to_dsfbase(drop_full_screen = TRUE,
                       dye_screen_dirs["Exp0596--H104-FL-B5-pre-inc_protein5"],
                   save_path = .save_path, # min_range = .min_range,
                    # save_data = .save_data, 
                    ...)

#Exp0598_FL-B5-pre-inc_protein2
  dye_screen_to_dsfbase(dye_screen_dirs["Exp0598--FL-B5-pre-inc_protein2"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_noncanon = c("A10_TAMRA_protein", "D8_TAMRA_protein", "H12_FAM_protein", "H23_FAM_protein",    "H5_ROX_protein", "I14_FAM_protein", "I15_FAM_protein", "I18_FAM_protein", "I24_TAMRA_protein",    "K5_Cy5_protein", "L2_ROX_protein", "L3_ROX_protein"),
   drop_from_SYPRO_noncanon = c("A1_TAMRA_protein", "M7_TAMRA_protein"),
   ...)

  #Exp0610_B5_with_zinc
  dye_screen_to_dsfbase(dye_screen_dirs["Exp0610--B5_with_zinc"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_noncanon = c("A24_FAM_protein", "B6_TAMRA_protein", "E14_Cy5.5_protein", "G7_FAM_protein", "K20_ROX_protein"),
   drop_from_SYPRO_noncanon = c(),
   ...
   )

  #BSA
  dye_screen_to_dsfbase(dye_screen_dirs["Exp0615--BSA"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   add_to_canon = c( "G19_FAM_protein"),
   drop_from_canon = c( "E15_Cy5_protein", "H14_Cy5.5_protein", "K21_TAMRA_protein"),
   add_to_noncanon = c("E15_Cy5_protein","H14_Cy5.5_protein","K21_TAMRA_protein"),
   drop_from_noncanon = c("B4_ROX_protein", "B6_TAMRA_protein", "B7_TAMRA_protein", "E20_TAMRA_protein", "E22_ROX_protein", "F16_FAM_protein",    "G19_FAM_protein", "K19_ROX_protein","K20_ROX_protein", "K24_FAM_protein", "K21_TAMRA_protein",    "L3_TAMRA_protein"),
   ...)

  #FKBP12
   dye_screen_to_dsfbase(dye_screen_dirs["Exp0616_FKBP12"],
                      save_path = .save_path, # min_range = .min_range,
                       # save_data = .save_data,
                       drop_from_noncanon = c("A10_TAMRA_protein", "E14_Cy5_protein", "E22_ROX_protein", "K20_ROX_protein"),
                       ...)

#PPIE
  dye_screen_to_dsfbase(dye_screen_dirs["Exp0619--PPIE"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   add_to_gotcha = c("A24_FAM_protein"),
   drop_from_canon = c("A24_FAM_protein",    "E1_FAM_protein",    "H12_FAM_protein",    "K24_ROX_protein"),
   add_to_noncanon = c("K21_TAMRA_protein","H12_FAM_protein","K24_ROX_protein"),
   drop_from_noncanon = c("A10_TAMRA_protein", "A20_FAM_protein", "F16_FAM_protein", "G17_TAMRA_protein",    "G7_FAM_protein", "H19_Cy5.5_protein", "K20_ROX_protein", "K4_Cy5.5_protein", "L5_Cy5_protein"),
   ...)

  #NPAS2 PAS A
  dye_screen_to_dsfbase(dye_screen_dirs["Exp0620--NPAS2_PAS-A"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_canon = c("B6_TAMRA_protein","H12_FAM_protein"),
   add_to_noncanon = c("B6_TAMRA_protein"),
   drop_from_noncanon = c("F16_FAM_protein", "G11_FAM_protein",    "L18_FAM_protein", "K20_ROX_protein", "K4_Cy5.5_protein"),
   ...)

  #Per2 PAS AB wt
  dye_screen_to_dsfbase(dye_screen_dirs["Exp0621--PER2_PAS-AB_wt"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_canon = c("F4_TAMRA_protein","L9_FAM_protein","L11_Cy5_protein", "L23_FAM_protein","L5_TAMRA_protein","L7_ROX_protein","L9_Cy5.5_protein","N7_FAM_protein", "N7_TAMRA_protein"),
   add_to_noncanon = c("F4_TAMRA_protein","L11_Cy5_protein", "L23_FAM_protein","L5_TAMRA_protein","L9_Cy5.5_protein", "N7_TAMRA_protein"),
   drop_from_noncanon = c("F22_Cy5_protein", "F23_Cy5_protein", "F24_Cy5_protein","K10_FAM_protein", "K9_FAM_protein", "L8_Cy5_protein", "P1_FAM_protein"),
   ...)

  #Exp0622_CLOCK_PASB_W362A
  dye_screen_to_dsfbase(dye_screen_dirs["Exp0622--CLOCK_PASB_W362A"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_canon = c("H12_FAM_protein", "K19_ROX_protein", "K21_TAMRA_protein"),
   add_to_noncanon = c("K19_ROX_protein", "K21_TAMRA_protein"),
   drop_from_noncanon = c("K20_ROX_protein", "F16_FAM_protein","G21_FAM_protein", "G7_FAM_protein", "G8_FAM_protein", "H19_Cy5.5_protein", "L18_FAM_protein", "L3_ROX_protein"),
   add_to_gotcha = c("H12_FAM_protein","G21_FAM_protein"),
   ...)

  #Exp0623_CP_BMAL1_PASB_W427A
  dye_screen_to_dsfbase(dye_screen_dirs["Exp0623--CP_BMAL1_PASB_W427A"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_canon = c("A24_FAM_protein","H12_FAM_protein", "K21_TAMRA_protein"),
   add_to_noncanon = c("K21_TAMRA_protein"),
   drop_from_noncanon = c("F16_FAM_protein","G11_FAM_protein","G17_TAMRA_protein","K24_ROX_protein","L5_Cy5_protein", "K19_ROX_protein"),
   add_to_gotcha = c("A24_FAM_protein","H12_FAM_protein","G11_FAM_protein","G17_TAMRA_protein","K24_ROX_protein"),
   ...)

  #Exp0624_HisPER2_AB_I324N
  dye_screen_to_dsfbase(dye_screen_dirs["Exp0624--HisPER2_AB_I324N"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_canon = c("A20_FAM_protein","B9_TAMRA_protein","E15_Cy5.5_protein","E22_ROX_protein","H12_FAM_protein","H15_Cy5.5_protein", "K21_TAMRA_protein", "K24_ROX_protein"),
   add_to_noncanon = c("A20_FAM_protein","B9_TAMRA_protein","E15_Cy5.5_protein","E22_ROX_protein","H15_Cy5.5_protein", "K21_TAMRA_protein", "K24_ROX_protein"),
   drop_from_noncanon = c("A24_FAM_protein"),
   add_to_gotcha = c("A24_FAM_protein"),
   ...)

  #Exp0625_CP581_CBPKIX
  dye_screen_to_dsfbase(dye_screen_dirs["Exp0625--CP581_CBPKIX"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_noncanon = c("F16_FAM_protein","K20_ROX_protein"),
   ...)

  #Exp0627_HisSUMO #no modifications
  dye_screen_to_dsfbase(dye_screen_dirs["Exp0627--HisSUMO"],
                     save_path = .save_path, # min_range = .min_range,
                      # save_data = .save_data, 
                      ...)

  #Hsp10
  dye_screen_to_dsfbase(dye_screen_dirs["Exp0628--Hsp10"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_canon = c("A24_FAM_protein"),
   add_to_noncanon = c("A24_FAM_protein"),
   drop_from_noncanon = c("G2_FAM_protein", "G7_FAM_protein",    "K20_ROX_protein"),
   ...)

  #Exp0630_Hsp60
  dye_screen_to_dsfbase(dye_screen_dirs["Exp0630_Hsp60"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_canon = c("A24_FAM_protein","B5_TAMRA_protein","B6_TAMRA_protein", "E15_Cy5.5_protein","H12_FAM_protein"),
   add_to_noncanon = c("B5_TAMRA_protein","B6_TAMRA_protein"),
   drop_from_noncanon = c("A10_TAMRA_protein","G17_TAMRA_protein",    "G22_ROX_protein","K20_ROX_protein", "K4_Cy5.5_protein"),
   add_to_gotcha = c("A24_FAM_protein","E15_Cy5.5_protein"),
   ...)

  #Exp0650_His_NusA
  dye_screen_to_dsfbase(dye_screen_dirs["Exp0650--His_NusA"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_canon = c("L3_TAMRA_protein","A24_FAM_protein", "K21_TAMRA_protein","B6_TAMRA_protein","H12_FAM_protein","L2_Cy5_protein"),
   add_to_noncanon = c("A24_FAM_protein", "K21_TAMRA_protein","B6_TAMRA_protein","L3_TAMRA_protein"),
   drop_from_noncanon = c("L18_FAM_protein","K20_ROX_protein", "L18_FAM_protein", "K5_Cy5_protein"),
   add_to_gotcha = c("K5_Cy5_protein"),
   ...)

  #Exp0671_GS_WT
  dye_screen_to_dsfbase(dye_screen_dirs["Exp0671--GS_WT"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_canon = c("K21_TAMRA_protein","B16_ROX_protein", "B2_TAMRA_protein", "B9_TAMRA_protein","E14_Cy5.5_protein","E2_FAM_protein","H12_FAM_protein","L3_TAMRA_protein","K24_ROX_protein","L2_Cy5_protein"),
   add_to_noncanon = c("K21_TAMRA_protein","B16_ROX_protein", "B2_TAMRA_protein", "B9_TAMRA_protein","E14_Cy5.5_protein","E2_FAM_protein","L3_TAMRA_protein","K24_ROX_protein"),
   add_to_gotcha = c("A24_FAM_protein","H12_FAM_protein"),
   drop_from_noncanon = c("A10_TAMRA_protein", "A20_FAM_protein", "A24_FAM_protein","E22_ROX_protein", "F16_FAM_protein", "F20_FAM_protein", "G17_TAMRA_protein", "G21_FAM_protein", "G22_ROX_protein", "G7_FAM_protein", "G9_FAM_protein", "H19_Cy5.5_protein", "K1_Cy5_protein", "K11_Cy5_protein", "K20_ROX_protein", "K24_ROX_protein"),
   ...)

  #Exp0672_GS_R341C
  dye_screen_to_dsfbase(dye_screen_dirs["Exp0672--GS_R341C"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_canon = c("H12_FAM_protein","L2_Cy5_protein","E2_FAM_protein","B9_TAMRA_protein"),
   add_to_noncanon = c("L2_Cy5_protein","E2_FAM_protein","B9_TAMRA_protein"),
   drop_from_noncanon = c("A10_ROX_protein", "A24_FAM_protein", "G7_FAM_protein","K1_Cy5_protein", "K4_Cy5.5_protein","L18_FAM_protein", "L5_Cy5_protein"), 
   ...)

  #Exp0673_GS_324S
  dye_screen_to_dsfbase(dye_screen_dirs["Exp0673--GS_324S"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_canon = c("B2_TAMRA_protein", "B4_ROX_protein","B9_TAMRA_protein", "E14_Cy5.5_protein", "E2_FAM_protein",
                                           "H12_FAM_protein",
                                           "K24_ROX_protein", "L2_Cy5_protein", "L2_ROX_protein"),
   add_to_noncanon =    c("B2_TAMRA_protein", "B4_ROX_protein","B9_TAMRA_protein", "E14_Cy5.5_protein", "E2_FAM_protein",
                                           "K24_ROX_protein", "L2_Cy5_protein", "L2_ROX_protein"),
   drop_from_noncanon = c("A10_ROX_protein", "A24_FAM_protein","E22_ROX_protein", "F16_FAM_protein", "F20_FAM_protein", "G21_FAM_protein", "G22_ROX_protein", "G7_FAM_protein", "H19_Cy5.5_protein", "H22_Cy5_protein", "K1_Cy5_protein","N6_FAM_protein"), 
   ...)

  #Exp0674_GS_324C
  dye_screen_to_dsfbase(dye_screen_dirs["Exp0674--GS_324C"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_canon = c("B2_TAMRA_protein", "B4_ROX_protein","B9_TAMRA_protein","E14_Cy5.5_protein","E2_FAM_protein","H12_FAM_protein", "K19_ROX_protein", "K21_TAMRA_protein", "K24_ROX_protein", "L2_ROX_protein", "L2_Cy5_protein"),
   add_to_noncanon = c("B2_TAMRA_protein", "B4_ROX_protein","B9_TAMRA_protein","E14_Cy5.5_protein","E2_FAM_protein", "K19_ROX_protein", "K21_TAMRA_protein", "K24_ROX_protein", "L2_ROX_protein"),
   drop_from_noncanon = c("A10_ROX_protein","A24_FAM_protein","F15_FAM_protein", "F20_FAM_protein","G22_ROX_protein","H19_Cy5.5_protein","K1_Cy5_protein","K20_ROX_protein", "K4_Cy5.5_protein", "K5_Cy5_protein", "L5_Cy5_protein"), 
   ...)

  #Exp0701_B5-pre-inc_phosphomim_protein_4
  dye_screen_to_dsfbase(   dye_screen_dirs["Exp0701--B5-pre-inc_phosphomim_protein_4"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_noncanon = c("H12_FAM_protein","K1_Cy5_protein","K19_TAMRA_protein", "K21_TAMRA_protein"), 
   ...)

  #nucleosome
  dye_screen_to_dsfbase(dye_screen_dirs["Exp0710--Nucleosome"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_canon = c("A24_FAM_protein", "B3_FAM_protein", "G22_ROX_protein"),
   move_to_noncanon = c("A24_FAM_protein", "B3_FAM_protein", "G22_ROX_protein"),
   drop_from_noncanon = c("E13_ROX_protein","E15_Cy5.5_protein","H12_FAM_protein","H5_TAMRA_protein", "K19_TAMRA_protein","L3_ROX_protein", "K24_ROX_protein"),
   add_to_gotcha = c("H12_FAM_protein","H5_TAMRA_protein","K19_TAMRA_protein","L3_ROX_protein"), 
   ...)  ## TODO -- why is there a hanging quotation mark here --  "

#Exp0723_Retinoic_acid_receptor_alpha
  dye_screen_to_dsfbase(dye_screen_dirs["Exp0723--Retinoic_acid_receptor_alpha"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_canon = c("K22_ROX_protein","K21_ROX_protein","H14_Cy5.5_protein","A24_FAM_protein"),
   move_to_noncanon = c("K22_ROX_protein","K21_ROX_protein","H14_Cy5.5_protein","A24_FAM_protein"),
   drop_from_noncanon = c("A10_TAMRA_protein","E21_FAM_protein","G21_FAM_protein", "G22_ROX_protein", "G7_FAM_protein","L5_Cy5_protein"),
   add_to_gotcha = c("E21_FAM_protein","G21_FAM_protein", "G22_ROX_protein", "G7_FAM_protein","L5_Cy5_protein"), 
   ...)

  #Exp0724_OGTase
  dye_screen_to_dsfbase(dye_screen_dirs["Exp0724_OGTase"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_canon = c("H12_FAM_protein"),
   drop_from_noncanon = c("G21_FAM_protein"),
   add_to_gotcha = c("G21_FAM_protein"), 
   ...)   ## TODO -- why is there a hanging quotation mark here --  "

#Exp0732_Widom_601_DNA
  dye_screen_to_dsfbase(dye_screen_dirs["Exp0732--Widom_601_DNA"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_canon = c("B3_FAM_protein", "D14_TAMRA_protein", "E12_ROX_protein", "E13_ROX_protein", "E14_Cy5.5_protein", "E19_FAM_protein", "E21_FAM_protein", "E23_ROX_protein", "H12_FAM_protein", "H14_Cy5.5_protein", "K17_TAMRA_protein", "K19_TAMRA_protein", "K21_TAMRA_protein", "K22_Cy5_protein", "K23_Cy5_protein", "K24_ROX_protein", "L1_Cy5_protein", "L2_ROX_protein", "L3_TAMRA_protein"),
   add_to_noncanon = c("B3_FAM_protein", "D14_TAMRA_protein", "E12_ROX_protein", "E13_ROX_protein", "E14_Cy5.5_protein", "E19_FAM_protein",    "E23_ROX_protein",    "H14_Cy5.5_protein",    "K22_Cy5_protein", "K23_Cy5_protein", "K24_ROX_protein", "L1_Cy5_protein", "L2_ROX_protein"),
   drop_from_noncanon = c("G17_TAMRA_protein","H23_Cy5_protein","K4_Cy5.5_protein"),
   add_to_gotcha = c("E21_FAM_protein","H12_FAM_protein","K17_TAMRA_protein", "K19_TAMRA_protein", "K21_TAMRA_protein","L3_TAMRA_protein","G17_TAMRA_protein","H23_Cy5_protein"), 
   ...)

#Exp0850_nsp3_mac1
  dye_screen_to_dsfbase(dye_screen_dirs["Exp0850_nsp3_mac1"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_canon = c("F20_ROX_protein", "F20_TAMRA_protein",    "M6_FAM_protein"),
      add_to_noncanon = c("F20_ROX_protein", "F20_TAMRA_protein",    "M6_FAM_protein"), 
   ...)

#Exp0891_nsp16_nsp10
  dye_screen_to_dsfbase(dye_screen_dirs["Exp0891_nsp16_nsp10"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_canon = c("I23_TAMRA_protein","I24_TAMRA_protein","I4_TAMRA_protein","K1_FAM_protein",    "M7_TAMRA_protein"),
   add_to_noncanon = c("I23_TAMRA_protein","I24_TAMRA_protein"),
   drop_from_noncanon = c("K3_FAM_protein"),
   add_to_gotcha = c("K3_FAM_protein", "I4_TAMRA_protein","K1_FAM_protein",    "M7_TAMRA_protein"), 
   ...)

#Exp0891_nsp16
  dye_screen_to_dsfbase(dye_screen_dirs["Exp0891_nsp16"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_canon = c("A4_TAMRA_protein","A23_TAMRA_protein",
                                           "A5_Cy5_protein","A6_Cy5_protein","C13_FAM_protein", "C13_JOE_protein","E8_FAM_protein","C7_TAMRA_protein"),
   add_to_canon = c("C17_TAMRA_protein"),
   add_to_noncanon = c("A5_Cy5_protein","A6_Cy5_protein","C13_FAM_protein", "C13_JOE_protein","E8_FAM_protein","C7_TAMRA_protein"),
   drop_from_noncanon    = c("A24_TAMRA_protein","A7_FAM_protein","C3_FAM_protein","E13_FAM_protein"),
   drop_from_SYPRO_canon = c("E5_TAMRA_protein"),
   add_to_gotcha = c("A4_TAMRA_protein","A23_TAMRA_protein","A24_TAMRA_protein","C3_FAM_protein"), 
   ...)

  #Exp0892_nsp10
  dye_screen_to_dsfbase(dye_screen_dirs["Exp0892_nsp10"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_canon = c("A5_Cy5_protein","E7_FAM_protein"),
   add_to_noncanon = c("A5_Cy5_protein","E7_FAM_protein"),
   drop_from_noncanon = c( "C6_TAMRA_protein")
   )

  #Exp0897_snf2h
  dye_screen_to_dsfbase(dye_screen_dirs["Exp0897_snf2h"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_canon = c("A23_ROX_protein", "A23_TAMRA_protein","A6_ROX_protein"),
   add_to_noncanon = c("A23_ROX_protein","A6_ROX_protein"),
   drop_from_noncanon = c("A24_TAMRA_protein","C11_Cy5_protein","E11_Cy5_protein","E17_Cy5_protein","E20_Cy5_protein","E8_FAM_protein","H2_FAM_protein","I3_JOE_protein","I4_Cy5_protein", "M19_TAMRA_protein", "M20_TAMRA_protein", "M6_JOE_protein"),
   add_to_gotcha = c("A24_TAMRA_protein","I3_JOE_protein","I4_Cy5_protein"), 
   ...)

  #Exp0900_nsp14_nsp10
  dye_screen_to_dsfbase(dye_screen_dirs["Exp0900_nsp14_nsp10"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_SYPRO_canon = c("M5_TAMRA_protein"),
   drop_from_canon = c("I23_TAMRA_protein","I4_TAMRA_protein","I5_Cy5_protein","I6_Cy5_protein","I8_Cy5_protein","K3_FAM_protein"),
   add_to_noncanon = c( "I5_Cy5_protein","I8_Cy5_protein"),
   add_to_gotcha = c( "I23_TAMRA_protein","M5_TAMRA_protein", "I4_TAMRA_protein","K3_FAM_protein"), 
   ...)

  #proteasome
  dye_screen_to_dsfbase(dye_screen_dirs["Exp0933_proteasome"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_noncanon = c("A11_TAMRA_protein", "A15_Cy5_protein", "A22_Cy5_protein", "A4_TAMRA_protein", "A6_ROX_protein", "C11_Cy5_protein", "C13_TAMRA_protein", "C14_TAMRA_protein", "C2_JOE_protein", "C3_FAM_protein", "C4_TAMRA_protein", "C7_TAMRA_protein"), 
   ...)

  #Exp0937_pp5
  dye_screen_to_dsfbase(dye_screen_dirs["Exp0937_pp5"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_canon = c("A12_TAMRA_protein"),
   drop_from_noncanon = c("C14_TAMRA_protein", "E7_JOE_protein", "H16_TAMRA_protein", "I1_FAM_protein", "I4_Cy5_protein", "J1_FAM_protein"),
   add_to_gotcha = c("H16_TAMRA_protein", "I1_FAM_protein", "I4_Cy5_protein", "J1_FAM_protein"), 
   ...)

  #Exp0945_MPro
  dye_screen_to_dsfbase(dye_screen_dirs["Exp0945_MPro"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_canon = c("E5_Cy5_protein", "G14_TAMRA_protein","G3_FAM_protein"),
   add_to_noncanon = c("E5_Cy5_protein", "G14_TAMRA_protein","G3_FAM_protein"), 
   ...)

# nucleocapsid
  dye_screen_to_dsfbase(dye_screen_dirs["Exp0945_nucleocapsid"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_canon = c("I5_Cy5_protein", "I5_ROX_protein", "K11_Cy5_protein", "K11_ROX_protein"),
   add_to_noncanon = c("I5_Cy5_protein", "I5_ROX_protein", "K11_Cy5_protein"),
   drop_from_noncanon = c("K3_FAM_protein","I7_FAM_protein","I6_Cy5_protein","I19_Cy5_protein"), 
   ...)

# Exp0945_PLPro
  dye_screen_to_dsfbase(dye_screen_dirs["Exp0945_PLPro"],
                     save_path = .save_path, # min_range = .min_range,
                      # save_data = .save_data,
                      drop_from_canon = c("A12_Cy5_protein", "A23_TAMRA_protein","A23_ROX_protein","A4_TAMRA_protein","A5_ROX_protein", "A8_Cy5_protein", "C13_JOE_protein", "C14_TAMRA_protein","C18_TAMRA_protein","C3_FAM_protein", "C4_ROX_protein", "C4_TAMRA_protein", "C5_TAMRA_protein", "C7_TAMRA_protein"),
                      add_to_noncanon = c("A12_Cy5_protein", "A23_ROX_protein","A4_TAMRA_protein","A5_ROX_protein", "A8_Cy5_protein", "C13_JOE_protein", "C14_TAMRA_protein","C18_TAMRA_protein","C3_FAM_protein", "C4_ROX_protein", "C4_TAMRA_protein", "C5_TAMRA_protein", "C7_TAMRA_protein"),
                      drop_from_noncanon = c("A11_TAMRA_protein",  "C11_Cy5_protein"), 
                      ...)

  #Exp0947_hACE2
  dye_screen_to_dsfbase(dye_screen_dirs["Exp0947_hACE2"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_canon = c("I11_TAMRA_protein",    "I19_Cy5_protein", "I19_Cy5.5_protein",    "I22_Cy5_protein",    "I8_Cy5_protein"),
   add_to_noncanon = c("I11_TAMRA_protein",    "I19_Cy5_protein", "I19_Cy5.5_protein",    "I22_Cy5_protein",    "I8_Cy5_protein"),
   drop_from_noncanon = c("I7_FAM_protein"), 
   ...)

  #Exp0947_RBD
  dye_screen_to_dsfbase(dye_screen_dirs["Exp0947_RBD"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_canon = c("C3_FAM_protein"),
   drop_from_noncanon = c("A8_Cy5_protein", "C12_TAMRA_protein", "C13_JOE_protein",    "E4_FAM_protein",    "E7_TAMRA_protein", "G11_FAM_protein", "G12_FAM_protein", "G7_ROX_protein"),
   add_to_gotcha = c("C3_FAM_protein"), 
   ...)

  #Exp0950_GST
  dye_screen_to_dsfbase(dye_screen_dirs["Exp0950_GST"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_canon = c("A2_ROX_protein"),
   drop_from_noncanon = c("A12_Cy5.5_protein", "A14_ROX_protein", "A16_ROX_protein", "A24_TAMRA_protein", "A6_ROX_protein", "E17_Cy5_protein", "E3_TAMRA_protein", "G11_TAMRA_protein", "G6_ROX_protein", "H22_TAMRA_protein", "H6_ROX_protein",    "L1_TAMRA_protein"),
   add_to_gotcha = c("H6_ROX_protein"), 
   ...)

  #Exp0951_diUb
  dye_screen_to_dsfbase(dye_screen_dirs["Exp0951_diUb"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_noncanon = c("K3_FAM_protein"),
   add_to_gotcha = c( "K3_FAM_protein"), 
   ...)

  #Exp0951_ISG15
  dye_screen_to_dsfbase(dye_screen_dirs["Exp0951_ISG15"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_canon = c("A8_Cy5_protein", "A8_Cy5.5_protein", "C11_Cy5_protein", "C3_FAM_protein"),
   add_to_noncanon = c("A8_Cy5_protein", "A8_Cy5.5_protein", "C11_Cy5_protein", "C3_FAM_protein"),
   drop_from_noncanon = c("E17_Cy5_protein", "E20_Cy5_protein","E7_JOE_protein"), 
   ...)

  #Exp1062_mortalin
  dye_screen_to_dsfbase(dye_screen_dirs["Exp1062_mortalin"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_canon = c("E13_FAM_protein", "E7_TAMRA_protein", "K9_TAMRA_protein"),
   add_to_noncanon = c("E13_FAM_protein","K9_TAMRA_protein"),
   drop_from_noncanon = c( "H15_FAM_protein","H3_TAMRA_protein", "I22_ROX_protein", "I3_TAMRA_protein","J10_TAMRA_protein", "J3_TAMRA_protein"),
   add_to_gotcha = c( "I3_TAMRA_protein","H15_FAM_protein"), 
   ...)

  #Exp1076_nsp2
  dye_screen_to_dsfbase(dye_screen_dirs["Exp1076_nsp2"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_canon = c("C3_FAM_protein","A15_Cy5_protein","A23_ROX_protein","A23_TAMRA_protein","A5_Cy5_protein", "A5_ROX_protein", "C15_FAM_protein"),
   add_to_noncanon = c("C3_FAM_protein","A15_Cy5_protein","A23_ROX_protein","C15_FAM_protein"),
   drop_from_noncanon = c("A7_FAM_protein", "C14_TAMRA_protein"), 
   ...)

  #Exp1132_p23 #no modifications
  dye_screen_to_dsfbase(dye_screen_dirs["Exp1132_p23"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data, 
   ...)

  #Exp1152_nhr23_LBD
  dye_screen_to_dsfbase(dye_screen_dirs["Exp1152_nhr23_LBD"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_noncanon = c("I22_Cy5_protein", "I5_TAMRA_protein"), 
   ...)

  #Exp1194_CHIP
  dye_screen_to_dsfbase(dye_screen_dirs["Exp1194_CHIP"],
  save_path = .save_path, # min_range = .min_range,
   # save_data = .save_data,
   drop_from_canon = c("A18_Cy5_protein", "A23_TAMRA_protein"),
   add_to_noncanon = c("A18_Cy5_protein", "A23_TAMRA_protein"),
   drop_from_noncanon = c("A17_FAM_protein","A24_TAMRA_protein","C14_TAMRA_protein", "E11_Cy5_protein", "E17_Cy5_protein", "E20_Cy5_protein", "E8_FAM_protein", "G15_JOE_protein"), 
   ...)

  #Caspase screens
  #Exp1246_SP214_caspase7_active
    dye_screen_to_dsfbase(dye_screen_dirs["Exp1246--20211202_SP214_caspase7_active"],
    save_path = .save_path, # min_range = .min_range,
     # save_data = .save_data,
     is_caspase = TRUE,
     drop_from_canon = c("A4_TAMRA_protein", "C11_Cy5_protein","C3_FAM_protein", "E17_Cy5_protein", "E9_FAM_protein", "H20_Cy5_protein", "J15_JOE_protein"),
     add_to_noncanon = c("A4_TAMRA_protein", "C11_Cy5_protein","C3_FAM_protein", "E17_Cy5_protein", "E9_FAM_protein"),
     drop_from_noncanon = c("A13_ROX_protein","E8_FAM_protein"),
     drop_from_SYPRO_noncanon = c("E5_TAMRA_protein"), 
     ...)
  
    #Exp1256_SP215_caspase1_zymogen
    dye_screen_to_dsfbase(dye_screen_dirs["Exp1256--20211210_SP215_caspase1_zymogen"],
    save_path = .save_path, # min_range = .min_range,
     # save_data = .save_data,
     is_caspase = TRUE,
     drop_from_canon = c( "C3_FAM_protein", "A8_Cy5.5_protein"),
     add_to_gotcha = c( "C3_FAM_protein"),
     add_to_noncanon = c("A8_Cy5.5_protein"),
     drop_from_noncanon = c("A15_Cy5.5_protein","E11_Cy5_protein", "E13_FAM_protein", "E20_Cy5_protein", "E3_ROX_protein", "H19_Cy5_protein", "I24_Cy5_protein", "K23_Cy5_protein"), 
     ...)
 
     #Exp1257_SP216_caspase3_active
    dye_screen_to_dsfbase(dye_screen_dirs["Exp1257--20211210_SP216_caspase3_active"],
    save_path = .save_path, # min_range = .min_range,
     # save_data = .save_data,
     is_caspase = TRUE,
     drop_from_canon = c("A11_TAMRA_protein", "A12_Cy5_protein", "A24_TAMRA_protein","A6_ROX_protein", "A8_Cy5_protein", "C11_TAMRA_protein","C3_FAM_protein"),
     add_to_noncanon = c("A11_TAMRA_protein", "A12_Cy5_protein", "A24_TAMRA_protein","A6_ROX_protein",    "C11_TAMRA_protein"),
     drop_from_noncanon = c("A15_Cy5.5_protein", "A23_TAMRA_protein","H15_FAM_protein"), 
     ...)
  
    #Exp1258_SP217_caspase6_active
    dye_screen_to_dsfbase(dye_screen_dirs["Exp1258--20211210_SP217_caspase6_active"],
    save_path = .save_path, # min_range = .min_range,
     # save_data = .save_data,
     is_caspase = TRUE,
     drop_from_canon = c("A22_JOE_protein", "A23_TAMRA_protein", "A4_TAMRA_protein", "A5_ROX_protein","C20_Cy5.5_protein", "C3_FAM_protein", "E17_Cy5_protein", "H11_TAMRA_protein"),
     add_to_noncanon = c("A22_JOE_protein","A4_TAMRA_protein", "A5_ROX_protein","C20_Cy5.5_protein", "E17_Cy5_protein", "H11_TAMRA_protein"),
     drop_from_noncanon = c("P3_FAM_protein"),
     ...)
  
    #SP218_caspase9_active
    dye_screen_to_dsfbase(dye_screen_dirs["Exp1259--20211210_SP218_caspase9_active"],
    save_path = .save_path, # min_range = .min_range,
     # save_data = .save_data,
     is_caspase = TRUE,
     drop_from_canon = c("A11_TAMRA_protein","A23_TAMRA_protein", "A24_TAMRA_protein","C11_Cy5_protein", "C14_TAMRA_protein","C4_TAMRA_protein"),
     add_to_noncanon = c("A11_TAMRA_protein","C11_Cy5_protein", "C14_TAMRA_protein","C4_TAMRA_protein"),
     drop_from_noncanon = c("E11_Cy5_protein","E17_Cy5_protein", "E20_Cy5_protein","E8_FAM_protein"), 
     ...)
    
}
