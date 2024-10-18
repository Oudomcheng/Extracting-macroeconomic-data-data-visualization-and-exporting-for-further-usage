#==============================================================================
#========================Key Indicators for Australia==========================
#==============================================================================

# Load library package for IMF data extraction:
library(imf.data)

# see possible values for dimension 'ref_area', use "View()" easier to see
View(IFS$dimensions$ref_area) # Country names & codes

# Result: "AU" = Australia

# see possible values for dimension 'indicator'
View(IFS$dimensions$indicator) # Search codes to use in function get_series()

#==============================================================================
# Real sector:
#==============================================================================

# Using "get_series()" to extract data on Real Effective Exchange Rate for major countries 
library(imf.data);

#1. Nominal GDP   = "NGDP_NSA_XDC"
#2. Real GDP      = "NGDP_R_NSA_XDC"
#3. GDP Deflator  = "NGDP_D_IX"
#4. Inflation     = "PCPI_PC_CP_A_PT"

# Creating a row vector called "real_sector_indicators" to store real sector variable names
real_sector_indicators <- c("NGDP_NSA_XDC",
                            "NGDP_R_NSA_XDC",
                            "NGDP_D_IX",
                            "PCPI_PC_CP_A_PT")

AU_real_sector <- IFS$get_series(freq = "Q",
                                      ref_area = "AU",
                                      indicator = real_sector_indicators,
                                      start_period = "1960-01-01",
                                      end_period = "2024-12-01")

View(AU_real_sector)

  #Note: GDP deflator is not available. We obtained only 3 indicators;

  ## First we need to show names in our data.frame "AU_real_sector"
  names(AU_real_sector)
  
    # Results of showing names: [1] "TIME_PERIOD",
                                    # "Q.AU.PCPI_PC_CP_A_PT",
                                    # "Q.AU.NGDP_R_NSA_XDC",
                                    # "Q.AU.NGDP_NSA_XDC";
  
    # Using "rename()" function to rename our real sector variables
  library(tidyverse) # load library first
  
  AU_real_sector <- AU_real_sector %>% 
  dplyr::rename(
  "inflation"     = "Q.AU.PCPI_PC_CP_A_PT",   # new_name1 = old name,
  "real_gdp"      = "Q.AU.NGDP_R_NSA_XDC",    # new_name1 = old name,
  "nominal_gdp"   = "Q.AU.NGDP_NSA_XDC")      # new_name1 = old name,
  
  # Create "Real GDP growth rate (quarter on quarter)":
  library(tidyverse)
  library(dplyr)
  
  AU_real_sector <- AU_real_sector %>% 
    dplyr::mutate(real_gdp_qoq = (real_gdp / lag(real_gdp) - 1) * 100)
  
  AU_real_sector <- AU_real_sector %>% 
    dplyr::mutate(nominal_gdp_qoq = (nominal_gdp / lag(nominal_gdp) - 1) * 100)
  
  head(AU_real_sector)
  
  # Calcualte Annual percentage change (annual growth of real gdp) AU_real_sector with a 'real_gdp' column and a 'date' column:
  
    # First, make sure the data is sorted by date
    AU_real_sector <- AU_real_sector[order(AU_real_sector$TIME_PERIOD), ]
    
    # Calculate the year-on-year percentage change. Note on the function lag().
    ?lag()
    AU_real_sector$real_gdp_yoy <- (AU_real_sector$real_gdp / dplyr::lag(AU_real_sector$real_gdp, n = 4) - 1) * 100
    
    AU_real_sector$nominal_gdp_yoy <- (AU_real_sector$nominal_gdp / dplyr::lag(AU_real_sector$nominal_gdp, n = 4) - 1) * 100
    
    # View the data frame with the new column
    head(AU_real_sector)
    
  
#==============================================================================
  
  # Creating a potential GDP for Australia using one-sided HP filter (based on Kalman filter)
    # install hp-filter package command: install.packages("mFilter")
    # load package: 

  # Install and load the mFilter package
  # install.packages("mFilter", FORCE=TRUE)
  library(mFilter)
  
  # Explore the hpfilter documentation
  ?hpfilter
  
  # Extract the 'real_gdp' column
  AU_real_gdp <- AU_real_sector$real_gdp
  class(AU_real_gdp)
  
  # Ensure AU_real_gdp is a numeric vector (no need to convert it to a data frame for hpfilter)
  AU_real_gdp <- as.numeric(AU_real_gdp)
 
  sum(is.na(AU_real_gdp))
  AU_real_gdp[is.na(AU_real_gdp)]
  
  View(AU_real_gdp)
  AU_real_gdp <- data.frame(AU_real_gdp)
  AU_real_gdp[258,]=AU_real_gdp[257,]                    #sensitive to NA so we assume row 258 = 257
  
  # Apply the HP filter with the correct parameters
  AU_potential_gdp_filtered <- hpfilter(x = AU_real_gdp, freq = 1600)

  # View the result
  str(AU_potential_gdp_filtered)
  class(AU_potential_gdp_filtered) # = mfilter
  View(AU_potential_gdp_filtered)
  
  # Check the numerical in the mfilter file for our trend & cycle
  View(AU_potential_gdp_filtered$trend) # = this is potential gdp
  View(AU_potential_gdp_filtered$cycle)
  
  # Extract the trend and cyclical components
    AU_gdp_trend     <- AU_potential_gdp_filtered$trend
    class(AU_gdp_trend)
    AU_gdp_trend <- as.numeric(AU_gdp_trend)
    
    AU_gdp_cycle     <- AU_potential_gdp_filtered$cycle
    class(AU_gdp_cycle)
    
  # Combine the trend and cycle into the data frame
    AU_hp_filtered_gdp_data <- data.frame(AU_real_gdp, AU_gdp_trend, AU_gdp_cycle)
  
  # Rename to correct variables in our data frame:
    names(AU_hp_filtered_gdp_data)
    View(AU_hp_filtered_gdp_data)
    
    library(tidyverse)
    AU_hp_filtered_gdp_data <- AU_hp_filtered_gdp_data %>% 
      dplyr::rename(
        "real_gdp"       = "AU_real_gdp",
        "potential_gdp"  = "AU_gdp_trend",
        "cycle_gdp"      = "AU_gdp_cycle")
        
    View(AU_hp_filtered_gdp_data)
    
  # Combine potential gdp into "AU_real_sector"
    AU_real_sector <- data.frame(AU_real_sector, AU_gdp_trend)
    View(AU_real_sector)
  
  # Rename the newly included variable
    AU_real_sector <- AU_real_sector %>% 
      dplyr::rename("potential_gdp"  = "AU_gdp_trend")
        
    View(AU_real_sector)
    
  # Organizing columns in "AU_real_sector" data set:
    library(tidyverse)
    names(AU_real_sector)
    
    AU_real_sector <- AU_real_sector %>%
      dplyr::select(TIME_PERIOD, 
                    nominal_gdp, 
                    real_gdp,
                    potential_gdp,
                    inflation)
    View(AU_real_sector)
  
    str(AU_real_sector)
    
#===========================================================================

# Running a deterministic time trend regression on real gdp to get potential gdp:
# Ensure 'date' is in the Date format (if not already)
    
    library(tidyverse)
    library(dplyr)
    
    AU_real_sector <- AU_real_sector %>% 
      dplyr::mutate(TIME_PERIOD = seq.Date(
        from = as.Date("1960-01-01"), 
        to = as.Date("2024-04-01"), 
        by = "quarter"))
    
    glimpse(AU_real_sector)
    
    # Converting our real sector data frame to become numeric, except "TIME_PERIOD":
    AU_real_sector[, -1] <- lapply(AU_real_sector[, -1], as.numeric)
    
    # Check the structure of the data frame to confirm changes
    dplyr::glimpse(AU_real_sector)
    
# Step 2: Regress real_gdp on date (using date as a numerical value)
gdp_model <- lm(real_gdp ~ TIME_PERIOD, data = AU_real_sector)
AU_real_sector <- AU_real_sector[1:257,]
AU_real_sector$potential_gdp_det <- fitted(gdp_model)

View(AU_real_sector) 

library(ggplot2)
AU_real_sector %>% 
ggplot()+
  geom_line(aes(x=TIME_PERIOD, y=log(real_gdp), col="real_gdp"))+
  geom_line(linetype="dashed", 
            aes(x=TIME_PERIOD, y=log(potential_gdp_det), 
                                   col="potential_gdp"))+
    theme_classic();

#==============================================================================
# General Government Fiscal Accounts:
#==============================================================================

  # Using "get_series()" to extract data on Real Effective Exchange Rate for major countries 
  library(imf.data)
  View(FM.available.indicator)

  fiscal_sector_indicators <- c("GG_GXOB_G01_XDC",
                                "GG_GXCBG_G01_XDC",
                                "GG_GXCBN_G01_XDC",
                                "GG_GR_G01_XDC",
                                "GG_GRT_G01_XDC",
                                "GG_GRG_G01_XDC",
                                "GG_GEI_G01_XDC",
                                "GG_GEG_G01_XDC",
                                "GG_GEKC_G01_XDC",
                                "GG_GECE_G01_XDC",
                                "GG_GE_G01_XDC")
  
    ## Names of each indicators:
  
        # "GG_GXOB_G01_XDC" = Net lending/borrowing
        # "GG_GXCBG_G01_XDC" = Gross operating balance
        # "GG_GXCBN_G01_XDC" = Net operating balance
        # "GG_GR_G01_XDC" = Total Revenue
        # "GG_GRT_G01_XDC" = Tax Revenue
        # "GG_GRG_G01_XDC" = Grants
        # "GG_GEI_G01_XDC" = Interest Expense
        # "GG_GEG_G01_XDC" = Grants Expense
        # "GG_GEKC_G01_XDC" = Consumption of Fixed Capital (expense)
        # "GG_GECE_G01_XDC" = Compensation of Employees
        # "GG_GE_G01_XDC" = Total Expense
  
  
  AU_fiscal_sector <- IFS$get_series(freq = "Q",
                                   ref_area = "AU",
                                   indicator = fiscal_sector_indicators,
                                   start_period = "1960-01-01",
                                   end_period = "2024-12-01")
  
  View(AU_fiscal_sector)
  
  ## First we need to show names in our data.frame "AU_real_sector"
  names(AU_fiscal_sector)
  
  # Results of showing names:
   
  
  # Using "rename()" function to rename our real sector variables
  library(tidyverse) # load library first
  
  AU_fiscal_sector <- AU_fiscal_sector %>% 
    dplyr::rename(
      "net_lending"             = "Q.AU.GG_GXOB_G01_XDC",
      "gross_operating_balance" = "Q.AU.GG_GXCBG_G01_XDC",
      "net_operating_balance"   = "Q.AU.GG_GXCBN_G01_XDC",
      "total_revenue"           = "Q.AU.GG_GR_G01_XDC",
      "tax_revenue"             = "Q.AU.GG_GRT_G01_XDC",
      "grants"                  = "Q.AU.GG_GRG_G01_XDC",
      "interest_expense"        = "Q.AU.GG_GEI_G01_XDC",
      "grant_expense"           = "Q.AU.GG_GEG_G01_XDC",
      "capital_expense"         = "Q.AU.GG_GEKC_G01_XDC",
      "wage_expense"            = "Q.AU.GG_GECE_G01_XDC",
      "total_expense"           = "Q.AU.GG_GE_G01_XDC")
  
  View(AU_fiscal_sector)
  
#==============================================================================
  # Compiling Australia's Selected Fiscal Sector Indicators:
#==============================================================================

  library(imf.data)
  View(FM.available.indicator)
  
  selected_fiscal_sector_indicators <- 
  c("GGCB_G01_PGDP_PT",
  "GGCBP_G01_PGDP_PT",
  "G_X_G01_GDP_PT",
  "G_XWDG_G01_GDP_PT",
  "GGXWDN_G01_GDP_PT",
  "GGXCNL_G01_GDP_PT",
  "GGXONLB_G01_GDP_PT",
  "GGR_G01_GDP_PT")
  
  ## Names of each indicators:
        # "GGCB_G01_PGDP_PT" =   Cyclically adjusted balance (% of potential GDP)
        # "GGCBP_G01_PGDP_PT" = Cyclically adjusted primary balance (% of potential GDP)
        # "G_X_G01_GDP_PT" = Expenditure (% of GDP)
        # "G_XWDG_G01_GDP_PT" = Gross debt (% of GDP)
        # "GGXWDN_G01_GDP_PT" = Net debt (% of GDP)
        # "GGXCNL_G01_GDP_PT" = Net lending/borrowing (also referred as overall balance, % of GDP)
        # "GGXONLB_G01_GDP_PT" = Primary net lending/borrowing (primary balance, % of GDP)
        # "GGR_G01_GDP_PT" = Revenue (% of GDP)
        # "All_Indicators" = All Indicators

  # We will calculate above indicators by adding to the "AU_fiscal_sector" data frame:
  
  # We would have to trim Fiscal Sector data from Q2-2024 to only Q1-2024:
  AU_fiscal_sector <- data.frame(AU_fiscal_sector[1:83,])
                                 
  View(AU_fiscal_sector)
  
  ## First, we will add nominal GDP to the fiscal sector file:
  AU_fiscal_sector <- data.frame(AU_fiscal_sector, AU_real_sector$nominal_gdp[175:257])
  
  AU_fiscal_sector <- AU_fiscal_sector %>% 
  rename("nominal_gdp" = "AU_real_sector.nominal_gdp.175.257.")
  
  library(dplyr)
  AU_fiscal_sector <- AU_fiscal_sector %>% mutate_all(as.numeric)
  glimpse(AU_fiscal_sector)
  
  AU_fiscal_sector$TIME_PERIOD <- seq(as.Date("2003/7/1"), as.Date("2024/1/1"), by = "quarter")

  # Re-order the columns by category:
  
  library(dplyr)
  AU_fiscal_sector <- AU_fiscal_sector %>% 
    select(
  "TIME_PERIOD",
  "interest_expense",
  "wage_expense",
  "grant_expense",
  "capital_expense",
  "total_expense"  ,        
  "tax_revenue" ,           
  "grants",
  "total_revenue",
  "gross_operating_balance",
  "net_operating_balance"  , 
  "net_lending"     ,        
  "nominal_gdp"   )         
      
  dplyr::glimpse(AU_fiscal_sector) # all in correct class now. Then we can calculate "selected indicators":
  
  # Expenditure (% of GDP)
  AU_fiscal_sector$expense_percent_gdp <- (AU_fiscal_sector$total_expense/AU_fiscal_sector$nominal_gdp)*100
  
  library(dplyr)
  AU_fiscal_sector <- AU_fiscal_sector %>% 
    dplyr::rename("expense2gdp" = "expense_percent_gdp")
  
  # Net lending/borrowing (also referred as overall balance, % of GDP)
  
  AU_fiscal_sector$net_lending2gdp <- (AU_fiscal_sector$net_lending/AU_fiscal_sector$nominal_gdp)*100
  
  # Revenue (% of GDP)
  
  AU_fiscal_sector$revenue2gdp <- (AU_fiscal_sector$total_revenue/AU_fiscal_sector$nominal_gdp)*100
  
  # Using "get_series()"  to extract data on "public debt":
  
  # Code: "GG_GALM_G01_XDC"
  # Fiscal, General Government, 
  # Assets and Liabilities, 
  # Debt (at Market Value), 
  # Classification of the stocks of assets and liabilities, 
  # 2001 Manual, Domestic Currency
  
  library(imf.data);
  AU_public_debt <- IFS$get_series(freq = "Q",
                                        ref_area = "AU",
                                        indicator = "GG_GALM_G01_XDC",
                                        start_period = "2017-01-01",
                                        end_period = "2024-01-01")
  
  View(AU_public_debt)
  
  names(AU_public_debt)
  
  library(dplyr)
  AU_public_debt <- AU_public_debt %>% 
    dplyr::rename("public_debt" = "Q.AU.GG_GALM_G01_XDC")
  
  AU_public_debt <- AU_public_debt %>%
  dplyr::mutate(TIME_PERIOD = seq.Date(
    from = as.Date("2017-01-01"), 
    to = as.Date("2024-01-01"), 
    by = "quarter"))
  
  library(dplyr)
  glimpse(AU_public_debt)
  
  AU_public_debt$public_debt <- as.numeric(AU_public_debt$public_debt)
  
  glimpse(AU_public_debt)
  
  AU_public_debt$public_debt_bln_usd <- AU_public_debt$public_debt/1000
  
  View(AU_public_debt)
  
  # Put nominal gdp into public debt data frame in order to create a public debt to gdp ratio:
  
  View(AU_public_debt)
  View(AU_real_sector)
  
  library(dplyr)
  AU_public_debt <- AU_public_debt %>% 
  dplyr::mutate(AU_real_sector$nominal_gdp[229:257])
  
  View(AU_public_debt)
  AU_public_debt <- AU_public_debt %>% 
    dplyr::rename("nominal_gdp" = "AU_real_sector$nominal_gdp[229:257]")
  
  AU_public_debt <- AU_public_debt %>% 
    dplyr::mutate(debt2gdp=public_debt/nominal_gdp*100)
  
#===============================================================================
# External sector:
#===============================================================================

  View(BOP.available.indicator)

  # "export_goods" = "BXG_BP6_USD"
  # Current Account, Goods and Services, Goods, Credit, US Dollars
  # 
  # "import_goods" = "BMG_BP6_USD"
  # Current Account, Goods and Services, Goods, Debit, US Dollars
  # 
  # "net_goods" = "BG_BP6_USD"
  # Current Account, Goods and Services, Goods, Net, US Dollars
  # 
  # "export_service" = "BXS_BP6_USD"
  # Current Account, Goods and Services, Services, Credit, US Dollars
  # 
  # "import_service" = "BMS_BP6_USD"
  # Current Account, Goods and Services, Services, Debit, US Dollars
  # 
  # "net_service" = "BS_BP6_USD"
  # Current Account, Goods and Services, Services, Net, US Dollars
  # 
  # "export_goods&service" = "BXGS_BP6_USD"
  # Current Account, Goods and Services, Credit, US Dollars
  # 
  # "export_goods&service" = "BMGS_BP6_USD"
  # Current Account, Goods and Services, Debit, US Dollars
  # 
  # "net_goods&service" = "BGS_BP6_USD"
  # Current Account, Goods and Services, Net, US Dollars
  
  AU_current_account_code <- c("BXG_BP6_USD",
                          "BMG_BP6_USD",
                          "BG_BP6_USD",
                          "BXS_BP6_USD",
                          "BMS_BP6_USD",
                          "BS_BP6_USD",
                          "BXGS_BP6_USD",
                          "BMGS_BP6_USD",
                          "BGS_BP6_USD")
                          
  library(imf.data)
  AU_current_account_data <- BOP$get_series(freq = "Q",
                                        ref_area = "AU",
                                        indicator = AU_current_account_code,
                                        start_period = "1960-01-01",
                                        end_period = "2024-12-01")
  
  View(AU_current_account_data)
  
  # Rename the series in the "AU_current_account_data"
  library(dplyr)
  
  names(AU_current_account_data)
  
  AU_current_account_data <- AU_current_account_data %>% 
    dplyr::rename("export_goods" = "Q.AU.BXG_BP6_USD",
                  "import_goods" = "Q.AU.BMG_BP6_USD",
                  "net_goods" = "Q.AU.BG_BP6_USD",
                  "export_service" = "Q.AU.BXS_BP6_USD",
                  "import_service" = "Q.AU.BMS_BP6_USD",
                  "net_service" = "Q.AU.BS_BP6_USD",
                  "export_goods&service" = "Q.AU.BXGS_BP6_USD",
                  "import_goods&service" = "Q.AU.BMGS_BP6_USD",
                  "net_goods&service" = "Q.AU.BGS_BP6_USD")

  View(AU_current_account_data)
  
  #1. Financial Account, Other Investment, Net Acquisition of Financial Assets, US Dollars
  #  "Other_Investment_Asset" = "BFOA_BP6_USD"
  
  #2. Financial Account, Other Investment, Net Incurrence of Liabilities, US Dollars
  #  "Other_Investment_Liability" = "BFOL_BP6_USD"
  
  #3. Financial Account, Other Investment, US Dollars
  # Other_Investment_Net = "BFO_BP6_USD"
  
  #4. Financial Account, Portfolio Investment, Net Acquisition of Financial Assets, US Dollars
  # "Portfolio_Asset" = "BFPA_BP6_USD"
  
  #5. Financial Account, Portfolio Investment, Net Incurrence of Liabilities, US Dollars
  # "Portfolio_Liability" = "BFPL_BP6_USD"
  
  #6. Financial Account, Portfolio Investment, US Dollars
  # "Portfolio_Net" = "BFP_BP6_USD"
  
  #7. Financial Account, Net Lending (+) / Net Borrowing (-) (Balance from Financial Account), Direct Investment, Net Acquisition of Financial Assets, US Dollars
  # "FDI_Asset" = "BFDA_BP6_USD"
  
  #8. Financial Account, Net Lending (+) / Net Borrowing (-) (Balance from Financial Account), Direct Investment, Net Incurrence of Liabilities, US Dollars
  # "FDI_Liability" = "BFDL_BP6_USD"
  
  #9. Financial Account, Net Lending (+) / Net Borrowing (-) (Balance from Financial Account), Direct Investment, US Dollars
  # "FDI_Net" = "BFD_BP6_USD"
  
  #10. Financial Account, Net Lending (+) / Net Borrowing (-) (Balance from Financial Account), US Dollars
  # "Financial_Account_Net" = "BF_BP6_USD"  
    
  #11. Financial Account, Reserve Assets, US Dollars
  # "Reserve_asset" = "BFRA_BP6_USD"
  
  AU_financial_account_code <- c("BFOA_BP6_USD",
                                 "BFOL_BP6_USD",
                                 "BFO_BP6_USD",
                                 "BFPA_BP6_USD",
                                 "BFPL_BP6_USD",
                                 "BFP_BP6_USD",
                                 "BFDA_BP6_USD",
                                 "BFDL_BP6_USD",
                                 "BFD_BP6_USD",
                                 "BF_BP6_USD",
                                 "BFRA_BP6_USD")
  
  library(imf.data)
  AU_financial_account_data <- BOP$get_series(freq = "Q",
                                            ref_area = "AU",
                                            indicator = AU_financial_account_code,
                                            start_period = "1960-01-01",
                                            end_period = "2024-12-01")
  
  View(AU_financial_account_data)
  
  # Rename the series in the "AU_current_account_data"
  library(dplyr)
  
  names(AU_financial_account_data)
  
  AU_financial_account_data <- AU_financial_account_data %>% 
    dplyr::rename("Other_Investment_Asset"     = "Q.AU.BFOA_BP6_USD",
                  "Other_Investment_Liability" = "Q.AU.BFOL_BP6_USD",
                  "Other_Investment_Net"       = "Q.AU.BFO_BP6_USD" ,
                  "Portfolio_Asset"            = "Q.AU.BFPA_BP6_USD",
                  "Portfolio_Liability"        = "Q.AU.BFPL_BP6_USD",
                  "Portfolio_Net"              = "Q.AU.BFP_BP6_USD" ,
                  "FDI_Asset"                  = "Q.AU.BFDA_BP6_USD",
                  "FDI_Liability"              = "Q.AU.BFDL_BP6_USD",
                  "FDI_Net"                    = "Q.AU.BFD_BP6_USD" ,
                  "Financial_Account_Net"      = "Q.AU.BF_BP6_USD"  , 
                  "Reserve_asset"              = "Q.AU.BFRA_BP6_USD")
  
  View(AU_financial_account_data)

  #===============================================================================
  #Other Indicators
  #===============================================================================
  
  MFS.available.codes <- IMFData::DataStructureMethod("MFS")
  
  View(IFS.available.indicators)
  
  # # Exchange Rates, Real Effective Exchange Rate based on Consumer Price Index, Index
  # "REER" = "EREER_IX"
  # 
  # #Labor Markets, Unemployment Rate, Percent
  # "Unemployment_Rate" = "LUR_PT"
  # 
  # #Monetary, Monetary Survey, Domestic Credit (Non-Standardized Presentation), 
  # "Domestic_Credit_LCY" = "32____XDC"
  # 
  # #Monetary, Monetary Survey, Domestic Credit (Non-Standardized Presentation), 
  # #US Dollars
  # "Domestic_Credit_USD" = "32____USD"
  # 
  # #Financial, Interest Rates, Lending Rate, Percent per annum
  # "Lending_Rate_LCY" = "FILR_PA"
  # 
  # #Financial, Interest Rates, Lending Rate, Foreign Currency, US Dollar, 
  # #Percent per Annum
  # "Lending_Rate_USD" = "FILR_FX_USD_PA"
  # 
  # #Financial, Interest Rates, Lending Rate, Foreign Currency, Percent per Annum
  # "Lending_Rate_Annual_USD" = "FILR_FX_PA"
  # 
  # #Financial, Interest Rates, Deposit Rate, Foreign Currency, US Dollar, Percent per Annum
  # "Deposit_Rate_USD" = "FIDR_FX_USD_PA"
  # 
  # #Financial, Interest Rates, Central Bank Borrowing Facility Rate
  # "CB_Borrow_facility_rate" = "FIBFR_PA"
  # 
  # #Financial, Interest Rates, Refinancing Rate, Percent per annum
  # "CB_Refinancing_rate" = "FIR_PA"
  # 
  # #Financial, Interest Rates, Savings Rate, Foreign Currency, Percent per Annum
  # "Saving_rate_USD" = "FISR_FX_PA"
  # 
  # #Financial, Interest Rates, Savings Rate, Percent per annum
  # "Saving_rate_LCY" = "FISR_PA"
  # 
  # #Financial, Interest Rates, 3-Month Interbank Interest, Percent per annum
  # "3month_interbank_rate" = "FII_3M_PA"
  # 
  # #Monetary, Banking Institutions, Demand, Time, Savings, 
  # #and Foreign Currency Deposits (Non-Standardized Presentation), Domestic Currency
  # "Demand_Time_Savings" = "25L___XDC"
  # 
  # #Monetary, Banking Institutions, Deposits (Non-Standardized Presentation), Domestic Currency
  # "Deposits_LCY" = "26B___XDC"
  # 
  # #Monetary, Banking Institutions, Private Sector Deposits (Non-Standardized Presentation), 
  # #Domestic Currency
  # "Private_Sector_Deposits" = "24__I_XDC"
  # 
  # #Monetary, Banking Institutions, Time, Savings, and Foreign Currency Deposits 
  # #(Non-Standardized Presentation), Domestic Currency
  # "Time_Savings_FCD_LCY" = "25____XDC"
  # 
  # #Monetary, Banking Institutions, Time, Savings, and Foreign Currency Deposits 
  # #(Non-Standardized Presentation), US Dollars
  # "Time_Savings_FCD_USD" = "25____USD"
  
  library(imf.data);
  AU_other_indicator_code <- c("EREER_IX",
                               "LUR_PT",
                               "32____XDC",
                               "32____USD",
                               "FILR_PA",
                               "FILR_FX_USD_PA",
                               "FILR_FX_PA",
                               "FIDR_FX_USD_PA",
                               "FIBFR_PA",
                               "FIR_PA",
                               "FISR_FX_PA",
                               "FISR_PA",
                               "FII_3M_PA",
                               "25L___XDC",
                               "26B___XDC",
                               "24__I_XDC",
                               "25____XDC",
                               "25____USD")
  
  AU_other_indicator_data <- IFS$get_series(freq = "Q",
                                            ref_area = "AU",
                                            indicator = AU_other_indicator_code,
                                            start_period = "1960-01-01",
                                            end_period = "2024-12-01")
  
  View(AU_other_indicator_data)
  
  names(AU_other_indicator_data)
  
  # "TIME_PERIOD"    
  # "Q.AU.EREER_IX"  
  # "Q.AU.32____XDC" 
  # "Q.AU.FISR_PA"   
  # "Q.AU.FILR_PA"   
  # "Q.AU.25____XDC"
  # "Q.AU.LUR_PT"
  
  library(dplyr)
  AU_other_indicator_data <- AU_other_indicator_data %>% 
    dplyr::rename("REER"                    = "Q.AU.EREER_IX",
                  "Unemployment_Rate"       = "Q.AU.LUR_PT",
                  "Domestic_Credit_LCY"     = "Q.AU.32____XDC",
                  "Lending_Rate_LCY"        = "Q.AU.FILR_PA",
                  "Saving_rate_LCY"         = "Q.AU.FISR_PA",
                  "Time_Savings_FCD_LCY"    = "Q.AU.25____XDC")
  
  View(AU_other_indicator_data)
  
#==============================================================================
  # Monetary & Financial Sector
#==============================================================================
  library(imf.data)
  
  View(MFS.available.codes[["CL_MFS Section of International Financial Statistics (IFS) Indicator -- NEW_MFS"]])
  MFS.available.codes2view <- MFS.available.codes[["CL_MFS Section of International Financial Statistics (IFS) Indicator -- NEW_MFS"]]
  
  View(MFS.available.codes2view)
  
  # "M0_monetary" = "FM0_XDC"
  # Monetary, M0, Domestic Currency
  # 
  # "M1_monetary" = "FM1_XDC"
  # Monetary, M1, Domestic Currency
  # 
  # "M2_monetary" = "FM2_XDC"
  # Monetary, M2, Domestic Currency
  # 
  # "M3_monetary" = "FM3_XDC"
  # Monetary, M3, Domestic Currency
  # 
  # "Base_money_monetary" = "FMA_XDC"
  # Monetary, Base Money, Domestic Currency
  # 
  # "CIC_monetary" = "FMBCD_XDC"
  # Monetary, Broad Money, Currency in Circulation Outside Depository Corporations, Domestic Currency
  # 
  # "Broad_Money_monetary" = "FMB_XDC"
  # Monetary, Broad Money, Domestic Currency
  # 
  # "money_monetary" = "FMM_XDC"
  # Monetary, Money, Domestic Currency
  # 
  # "Narrow_Money_monetary" = "FMN_XDC"
  # Monetary, Narrow Money, Domestic Currency
  # 
  # "Quasi_Money_monetary" = "FMQ_XDC"
  # Monetary, Quasi Money, Domestic Currency
  # 
  # "Reserve_Money_monetary" = "FMR_XDC"
  # Monetary, Reserve Money, Domestic Currency
  # 
  # "Claims_on_Private_Sector" = "32D___XDC"
  # Monetary, Monetary Survey, Claims on Private Sector (Non-Standardized Presentation), Domestic Currency
  # 
  # "Domestic_Credit" = "32____XDC"
  # Monetary, Monetary Survey, Domestic Credit (Non-Standardized Presentation), Domestic Currency
  # 
  # "Foreign_Assets_Net" = "31N___XDC"
  # Monetary, Monetary Survey, Foreign Assets, Net (Non-Standardized Presentation), Domestic Currency
  # 
  # "Foreign_Liabilities" = "36CL__XDC"
  # Monetary, Monetary Survey, Foreign Liabilities, Long-term (Non-Standardized Presentation), Domestic Currency
  # 
  # "Money" = "34____XDC"
  # Monetary, Monetary Survey, Money (Non-Standardized Presentation), Domestic Currency
  # 
  # "Quasi_Money" = "35____XDC"
  # Monetary, Monetary Survey, Quasi-Money (Non-Standardized Presentation), Domestic Currency
  # 
  # "Quasi_Money_No_FCD" = "35B___XDC"
  # Monetary, Monetary Survey, Quasi-Money, Excluding Foreign Currency Deposits (Non-Standardized Presentation), Domestic Currency
  # 
  # "Restricted_Deposits" = "36B___XDC"
  # Monetary, Monetary Survey, Restricted Deposits (Non-Standardized Presentation), Domestic Currency
  # 
  # "Securities_in_monetary" = "36A___XDC"
  # Monetary, Monetary Survey, Securities Other than Shares (Non-Standardized Presentation), Domestic Currency
  # 
  # "Currency_Issued" = "34A___XDC"
  # Monetary, Monetary Survey, Money, Currency Issued (Non-Standardized Presentation), Domestic Currency
  # 
  # "Demand_Deposits" = "34B_N_XDC"
  # Monetary, Monetary Survey, Money, Demand Deposits (Non-Standardized Presentation), Domestic Currency
  # 
  # "Valuation_Adjustment_monetary" = "37RV__XDC"
  # Monetary, Monetary Survey, Other Items (Net), Valuation Adjustment (Non-Standardized Presentation), Domestic Currency
  # 
  # "Other_Items_Net" = "37R___XDC"
  # Monetary, Monetary Survey, Other Items, Net (Non-Standardized Presentation), Domestic Currency
  
  
  AU_Monetary_indicator_code <- c("FM0_XDC",
                                  "FM1_XDC",
                                  "FM2_XDC",
                                  "FM3_XDC",
                                  "FMA_XDC",
                                  "FMBCD_XDC",
                                  "FMB_XDC",
                                  "FMM_XDC",
                                  "FMN_XDC",
                                  "FMQ_XDC",
                                  "FMR_XDC",
                                  "32D___XDC",
                                  "32____XDC",
                                  "31N___XDC",
                                  "36CL__XDC",
                                  "34____XDC",
                                  "35____XDC",
                                  "35B___XDC",
                                  "36B___XDC",
                                  "36A___XDC",
                                  "34A___XDC",
                                  "34B_N_XDC",
                                  "37RV__XDC",
                                  "37R___XDC")
  
  print(AU_Monetary_indicator_code)
  
  library(imf.data)
  AU_Monetary_indicator_data <- MFS$get_series(freq = "Q",
                                               ref_area = "AU",
                                               indicator = AU_Monetary_indicator_code,
                                               start_period = "1960-01-01",
                                               end_period = "2024-12-01")
  
  View(AU_Monetary_indicator_data)
  
  names(AU_Monetary_indicator_data)
  
  library(dplyr)
  AU_Monetary_indicator_data <- AU_Monetary_indicator_data %>% 
    dplyr::rename("Other_Items_Net"        = "Q.AU.37R___XDC",  
                  "Quasi_Money"            = "Q.AU.35____XDC",
                  "Base_money_monetary"  = "Q.AU.FMA_XDC",   
                  "CIC_monetary"         = "Q.AU.FMBCD_XDC", 
                  "Foreign_Assets_Net"     = "Q.AU.31N___XDC",
                  "Money"                  = "Q.AU.34____XDC", 
                  "Broad_Money_monetary" = "Q.AU.FMB_XDC",   
                  "Domestic_Credit"        = "Q.AU.32____XDC", 
                  "Claims_on_Private_Sector" = "Q.AU.32D___XDC")
  
  View(AU_Monetary_indicator_data)
  
  #==============================================================================
  # Check classes of our main files:
  #==============================================================================
  
    # Class of real sector  
    class(AU_real_sector)
    dplyr::glimpse(AU_real_sector)
    
    #==============================================================================
    
    # Class of fiscal sector  
    class(AU_fiscal_sector)
    dplyr::glimpse(AU_fiscal_sector)
    
    #==============================================================================
    
    # Class of current account  
    class(AU_current_account_data)
    dplyr::glimpse(AU_current_account_data)
    
    # Convert all columns except the first (date column) to numeric
    ?lapply()
    AU_current_account_data[, -1] <- lapply(AU_current_account_data[, -1], as.numeric)
    
    # Check the structure of the data frame to confirm changes
    dplyr::glimpse(AU_current_account_data)
    
    #==============================================================================
    
    # Class of financial account  
    class(AU_financial_account_data)
    dplyr::glimpse(AU_financial_account_data)
    
    # Convert all columns except the first (date column) to numeric
    ?lapply()
    AU_financial_account_data[, -1] <- lapply(AU_financial_account_data[, -1], as.numeric)
    
    # Check the structure of the data frame to confirm changes
    dplyr::glimpse(AU_financial_account_data)
    
    #==============================================================================
    
    # Class of monetary sector  
    class(AU_Monetary_indicator_data)
    dplyr::glimpse(AU_Monetary_indicator_data)
    
    # Convert all columns except the first (date column) to numeric
    ?lapply()
    AU_Monetary_indicator_data[, -1] <- lapply(AU_Monetary_indicator_data[, -1], as.numeric)
    
    # Check the structure of the data frame to confirm changes
    dplyr::glimpse(AU_Monetary_indicator_data)
    
    #==============================================================================
    
    # Class of other indicator  
    class(AU_other_indicator_data)
    dplyr::glimpse(AU_other_indicator_data)
    
    # Convert all columns except the first (date column) to numeric
    ?lapply()
    AU_other_indicator_data[, -1] <- lapply(AU_other_indicator_data[, -1], as.numeric)
    
    # Check the structure of the data frame to confirm changes
    dplyr::glimpse(AU_other_indicator_data)
    
    #==============================================================================
    # Convert all date series in each data frame into "date" variable-same format:
    #==============================================================================
    
    
    library(tidyverse)
    library(dplyr)
    
    AU_other_indicator_data <- AU_other_indicator_data %>% 
      dplyr::mutate(TIME_PERIOD = seq.Date(
        from = as.Date("1960-01-01"), 
        to = as.Date("2024-04-01"), 
        by = "quarter"))
    
    #==============================================================================
    
    library(tidyverse)
    library(dplyr)
    
    AU_financial_account_data <- AU_financial_account_data %>% 
      dplyr::mutate(TIME_PERIOD = seq.Date(
        from = as.Date("1989-01-01"), 
        to = as.Date("2024-01-01"), 
        by = "quarter"))
    
    #==============================================================================
    
    library(tidyverse)
    library(dplyr)
    
    AU_current_account_data <- AU_current_account_data %>% 
      dplyr::mutate(TIME_PERIOD = seq.Date(
        from = as.Date("1989-01-01"), 
        to = as.Date("2024-01-01"), 
        by = "quarter"))
    
    #==============================================================================
    
    library(tidyverse)
    library(dplyr)
    
    AU_Monetary_indicator_data <- AU_Monetary_indicator_data %>% 
      dplyr::mutate(TIME_PERIOD = seq.Date(
        from = as.Date("1960-01-01"), 
        to = as.Date("2024-04-01"), 
        by = "quarter"))
    
  #==============================================================================
    # Exporting regional trade data to excel:
  #==============================================================================
  
  # Exporting regional trade data to excel:
  
  setwd("C:/Users/OUDOM/OneDrive/Documents/1. Personal/Study Rstudio/Data for Practice") # to change directory after getting data
  library(openxlsx)
  
  # Create a new Excel file
  Australia_macro_data_xlsx <- openxlsx::createWorkbook()
  
  #1. Add a sheet for the "Real sector" data frame
  openxlsx::addWorksheet(Australia_macro_data_xlsx, "AU_real_sector")
  openxlsx::writeData(Australia_macro_data_xlsx,    "AU_real_sector", AU_real_sector)
  
  #2. Add a sheet for the "Fiscal sector" data frame
  openxlsx::addWorksheet(Australia_macro_data_xlsx, "AU_fiscal_sector")
  openxlsx::writeData(Australia_macro_data_xlsx,    "AU_fiscal_sector", AU_fiscal_sector)
  
  #3. Add a sheet for the "Current account" data frame
  openxlsx::addWorksheet(Australia_macro_data_xlsx, "AU_current_account_data")
  openxlsx::writeData(Australia_macro_data_xlsx,    "AU_current_account_data", AU_current_account_data)
  
  #4. Add a sheet for the "Financial account" data frame
  openxlsx::addWorksheet(Australia_macro_data_xlsx, "AU_financial_account_data")
  openxlsx::writeData(Australia_macro_data_xlsx,    "AU_financial_account_data", AU_financial_account_data)
  
  #5. Add a sheet for the "Monetary sector" data frame
  openxlsx::addWorksheet(Australia_macro_data_xlsx, "AU_Monetary_indicator_data")
  openxlsx::writeData(Australia_macro_data_xlsx,    "AU_Monetary_indicator_data", AU_Monetary_indicator_data)
  
  #6. Add a sheet for the "Other indicators" data frame
  openxlsx::addWorksheet(Australia_macro_data_xlsx, "AU_other_indicator_data")
  openxlsx::writeData(Australia_macro_data_xlsx,    "AU_other_indicator_data", AU_other_indicator_data)
  
  openxlsx::saveWorkbook(Australia_macro_data_xlsx, "Australia_macro_data.xlsx", overwrite = TRUE) # by this command, we will have a file name.
  
  # Now we can check our data in excel by telling "R" to call up our excel data as follows:
  
  file.show("C:/Users/OUDOM/OneDrive/Documents/1. Personal/Study Rstudio/Data for Practice/Australia_macro_data.xlsx")
  
  #==============================================================================
  
  # Viewing before building graphs:
  
  View(AU_real_sector)
  View(AU_fiscal_sector)
  View(AU_Monetary_indicator_data)
  View(AU_current_account_data)
  View(AU_other_indicator_data)
  
  #==============================================================================
  
  # Checking class of the data frames available in this folder:
  
  library(dplyr)
  dplyr::glimpse(AU_real_sector)
  dplyr::glimpse(AU_fiscal_sector)
  dplyr::glimpse(AU_Monetary_indicator_data)
  dplyr::glimpse(AU_current_account_data)
  dplyr::glimpse(AU_financial_account_data)
  
  #==============================================================================
    # Building Important Graphs for Real Sector:
  #==============================================================================
    
                  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                  
                  # Install and load necessary packages
                    # install.packages("mschart")
                    # install.packages("officer")
                    # install.packages("rvg", force = TRUE)
                  
                  # Loading library packages
                    library(mschart)
                    library(officer)
                    library(rvg)
                    library(dplyr)
                    library(ggplot2)
                  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                  
  #==============================================================================
  # Create Australia's GDP graph:
  
  library(dplyr)
  library(ggplot2)
  library(scales)
  
  AU_GDP_graph <- AU_real_sector %>% 
    
    ggplot() +
    
    geom_line(aes(x = TIME_PERIOD, 
                  y = nominal_gdp/1000, 
                  col = "Nominal GDP")) +
    
    geom_line(aes(x = TIME_PERIOD, 
                  y = real_gdp/1000,
                  col = "Real GDP")) +
    
    geom_point(aes(x = TIME_PERIOD,
                   y = real_gdp/1000,
                   col = "Real GDP"))+
    
    labs(title = "Australia's Nominal & Real GDP",
         subtitle = "In Billion USD, 2009Q1 - 2024Q1",
         color="Lengend",
         x = NULL, 
         y = NULL)+
    
    theme_classic() +
    
    theme(panel.background = element_rect(fill = "white"), 
          panel.grid.major = element_line(color = "white"))+
    
    theme(axis.text.x = element_text(size = 10, 
                                     angle = -90,
                                     hjust = 1,
                                     face = "bold"))+
    
    theme(axis.text.y = element_text(size = 10, 
                                     face = "bold"))+
    
    theme(plot.title = element_text(size = 15, 
                                    hjust = 0.5,
                                    color = "black",
                                    face = "bold"))+
    
    theme(plot.subtitle = element_text(size = 10, 
                                       hjust = 0.5, 
                                       color = "darkgrey", 
                                       face = "bold"))+
    
    theme(text = element_text(family = "Rubik"))+
    
    
    #theme(axis.title.x = element_text(size = 10, 
    # color = "slateblue",
    # vjust = 1,
    # face = "bold"))+
    
    #theme(axis.title.y = element_text(size = 10, 
    # color = "slateblue", 
    # hjust = 0.5, 
    # face = "bold"))+
    
    theme(legend.position = "bottom")+
    
    scale_x_date(limits = c(AU_real_sector$TIME_PERIOD[197], max(AU_real_sector$TIME_PERIOD)), 
                 breaks = "1 year", 
                 date_labels = "%Y-%m")+
    
    # Changing scale of Y-axis
    scale_y_continuous(limits = c(250, 700), n.breaks = 10)
  
  plot(AU_GDP_graph)
  
    #=============================================================================
    
    ## Create Australia's GDP growth (qoq) graph:
    
    library(dplyr)
    library(ggplot2)
    library(scales)
    
    # Create Australia's  graph
    AU_GDP_qoq_growth_graph <- AU_real_sector %>% 
      ggplot() +
      
      geom_line(aes(x = TIME_PERIOD, 
                    y = real_gdp_qoq), 
                color = "red", 
                lwd = 0.5) +
      
      geom_line(aes(x = TIME_PERIOD, 
                    y = nominal_gdp_qoq), 
                color = "blue", 
                lwd = 0.5) +
    
      labs(title = "Australia's Real GDP Growth Rate QoQ",
           subtitle = "In Percent, 2009Q1 - 2024Q1",
           x = NULL, 
           y = NULL) +
      
      theme_classic() +
      
      theme(panel.background = element_rect(fill = "white"), 
            panel.grid.major = element_line(color = "white"))+
      
      theme(axis.text.x = element_text(size = 10, 
                                       angle = -90, 
                                       face = "bold"))+
      
      theme(axis.text.y = element_text(size = 10, 
                                       face = "bold"))+
      
      theme(plot.title = element_text(size = 15, 
                                      hjust = 0.5,
                                      color = "black",
                                      face = "bold"))+
      
      theme(plot.subtitle = element_text(size = 10, 
                                         hjust = 0.5, 
                                         color = "darkgrey", 
                                         face = "bold"))+
      
      theme(text = element_text(family = "Rubik"))+
      
      theme(axis.title.x = element_text(size = 10, 
                                        color = "slateblue", 
                                        face = "bold"))+
      
      theme(axis.title.y = element_text(size = 10, 
                                        color = "slateblue", 
                                        hjust = 1, 
                                        face = "bold"))+
      
      theme(legend.position = "none") +
      
      scale_x_date(limits = c(AU_real_sector$TIME_PERIOD[197], max(AU_real_sector$TIME_PERIOD)), 
                   breaks = "1 year", 
                   date_labels = "%Y-%m")+
      
      # Changing scale of Y-axis
      scale_y_continuous(limits = c(-10, 10), n.breaks = 10)
    
    plot(AU_GDP_qoq_growth_graph)
    
    #=============================================================================
    
    ## Create Australia's GDP growth (YoY) graph:
    
    library(dplyr)
    library(ggplot2)
    library(scales)
    
    # Create Australia's  graph
    AU_GDP_yoy_growth_graph <- AU_real_sector %>% 
      ggplot() +
      
      geom_line(aes(x = TIME_PERIOD, 
                    y = real_gdp_yoy), 
                color = "red", 
                lwd = 0.5) +
      
      geom_line(aes(x = TIME_PERIOD, 
                    y = nominal_gdp_yoy), 
                color = "blue", 
                lwd = 0.5) +
      
      labs(title = "Australia's Real GDP Growth Rate YoY",
           subtitle = "In Percent, 2009Q1 - 2024Q1",
           x = NULL, 
           y = NULL) +
      
      theme_classic() +
      
      theme(panel.background = element_rect(fill = "white"), 
            panel.grid.major = element_line(color = "white"))+
      
      theme(axis.text.x = element_text(size = 10, 
                                       angle = -90, 
                                       face = "bold"))+
      
      theme(axis.text.y = element_text(size = 10, 
                                       face = "bold"))+
      
      theme(plot.title = element_text(size = 15, 
                                      hjust = 0.5,
                                      color = "black",
                                      face = "bold"))+
      
      theme(plot.subtitle = element_text(size = 10, 
                                         hjust = 0.5, 
                                         color = "darkgrey", 
                                         face = "bold"))+
      
      theme(text = element_text(family = "Rubik"))+
      
      theme(axis.title.x = element_text(size = 10, 
                                        color = "slateblue", 
                                        face = "bold"))+
      
      theme(axis.title.y = element_text(size = 10, 
                                        color = "slateblue", 
                                        hjust = 1, 
                                        face = "bold"))+
      
      theme(legend.position = "none") +
      
      scale_x_date(limits = c(AU_real_sector$TIME_PERIOD[197], max(AU_real_sector$TIME_PERIOD)), 
                   breaks = "1 year", 
                   date_labels = "%Y-%m")+
      
      # Changing scale of Y-axis
      scale_y_continuous(limits = c(-8, 20), n.breaks = 10)
    
    plot(AU_GDP_yoy_growth_graph)
    
    #=============================================================================
    ## Create an inflation graph:
    
    library(dplyr)
    library(ggplot2)
    library(scales)
    
    # Create Australia's  graph
    AU_inflation_graph <- AU_real_sector %>% 
      ggplot() +
      
      geom_line(aes(x = TIME_PERIOD, 
                    y = inflation), 
                color = "red", 
                lwd = 0.5) +
      labs(title = "Australia's Inflation Rate",
           subtitle = "In Percent, 2009Q1 - 2024Q1",
           x = NULL, 
           y = NULL) +
      
      theme_classic() +
      
      theme(panel.background = element_rect(fill = "white"), 
            panel.grid.major = element_line(color = "white"))+
      
      theme(axis.text.x = element_text(size = 10, 
                                       angle = -90, 
                                       face = "bold"))+
      
      
      theme(axis.text.y = element_text(size = 10, 
                                       face = "bold"))+
      
      theme(plot.title = element_text(size = 15, 
                                      hjust = 0.5,
                                      color = "black",
                                      face = "bold"))+
      
      theme(plot.subtitle = element_text(size = 10, 
                                         hjust = 0.5, 
                                         color = "darkgrey", 
                                         face = "bold"))+
      
      theme(text = element_text(family = "Rubik"))+
      
      
      theme(axis.title.x = element_text(size = 10, 
                                        color = "slateblue", 
                                        face = "bold"))+
      
      theme(axis.title.y = element_text(size = 10, 
                                        color = "slateblue", 
                                        hjust = 1, 
                                        face = "bold"))+
      
      theme(legend.position = "none") +
      
      scale_x_date(limits = c(AU_real_sector$TIME_PERIOD[197], max(AU_real_sector$TIME_PERIOD)), 
                   breaks = "1 year", 
                   date_labels = "%Y-%m")+
      
      
      # Changing scale of Y-axis
      scale_y_continuous(limits = c(-2, 10), n.breaks = 10);
    
    plot(AU_inflation_graph)
    
    #===========================================================================
    
    #===========================================================================
    ## Create graph for Fiscal Sector:
    #===========================================================================
    
    # Creating graph of All Expenditure Items:
    
    # Load necessary packages
    library(ggplot2)
    library(dplyr)
    library(scales)
    
    # Assuming AU_fiscal_sector already has the required columns including 'TIME_PERIOD', 'interest_expense',
    # 'grant_expense', 'capital_expense', 'wage_expense'
    
    # Convert the data to long format for stacking without reshaping the data frame directly
    AU_fiscal_expense_long <- AU_fiscal_sector %>%
      select(TIME_PERIOD, interest_expense, grant_expense, capital_expense, wage_expense) %>%
      tidyr::pivot_longer(cols = c(interest_expense, grant_expense, capital_expense, wage_expense),
                          names_to = "expense_type", values_to = "expense_value")
    
    # Filter data for the range 2009Q1 to 2024Q1
    AU_fiscal_expense_long <- AU_fiscal_expense_long %>%
      filter(TIME_PERIOD >= as.Date("2009-01-01") & TIME_PERIOD <= as.Date("2024-03-31"))
    
    # Create a stacked bar graph for quarterly data with x-axis interval adjustment
    AU_fiscal_expense_graph <-
    ggplot(AU_fiscal_expense_long, aes(x = TIME_PERIOD, y = expense_value, fill = expense_type)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(title = "Australia's Government Expenditures by Type", 
           subtitle = "In million dollars, 2009Q1 - 2024Q1",
           x = NULL, 
           y = NULL, 
           fill = "Expense Type") +
      
      theme_classic() +
      
      scale_fill_manual(values = c("interest_expense" = "#F8766D", 
                                   "grant_expense" = "#00BA38", 
                                   "capital_expense" = "#619CFF", 
                                   "wage_expense" = "#F564E3")) +
      scale_x_date(breaks = seq(as.Date("2009-01-01"), as.Date("2024-03-31"), by = "9 months"),
                   labels = date_format("%Y%m")) +  # Format as year-quarter
      
      theme(legend.position = c(0, 1),            # Position legend on the top-left corner
            legend.justification = c(0, 1),       # Anchor the legend to the top-left
            legend.text = element_text(size = 8, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
            axis.text.y = element_text(hjust = 1, face = "bold"),
            text = element_text(size = 10)) +
      
      theme(plot.title = element_text(size = 12, 
                                      hjust = 0.5,
                                      color = "black",
                                      face = "bold"))+
      
      theme(plot.subtitle = element_text(size = 10, 
                                         hjust = 0.5, 
                                         color = "darkgrey", 
                                         face = "bold"))
    
    plot(AU_fiscal_expense_graph)
    
    #===========================================================================
    
    # Creating graph of All Revenue Items:
    
    # Load necessary packages
    library(ggplot2)
    library(dplyr)
    library(scales)
    
    names(AU_fiscal_sector)
    
    # tax_revenue, grants.
    
    # Convert the data to long format for stacking without reshaping the data frame directly
    AU_fiscal_revenue_long <- AU_fiscal_sector %>%
      select(TIME_PERIOD, tax_revenue, grants) %>%
      tidyr::pivot_longer(cols = c(tax_revenue, grants),
                          names_to = "revenue_type", values_to = "revenue_value")
    
    # Filter data for the range 2009Q1 to 2024Q1
    AU_fiscal_revenue_long <- AU_fiscal_revenue_long %>%
      filter(TIME_PERIOD >= as.Date("2009-01-01") & TIME_PERIOD <= as.Date("2024-03-31"))
    
    # Create a stacked bar graph for quarterly data with x-axis interval adjustment
    AU_fiscal_revenue_graph <-
      ggplot(AU_fiscal_revenue_long, aes(x = TIME_PERIOD, y = revenue_value, fill = revenue_type)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(title = "Australia's Government Revenues by Type", 
           subtitle = "In million dollars, 2009Q1 - 2024Q1",
           x = NULL, 
           y = NULL, 
           fill = "Revenue Type") +
      
      theme_classic() +
      
      scale_fill_manual(values = c("tax_revenue" = "#F8766D", 
                                   "grants" = "#00BA38")) +
      scale_x_date(breaks = seq(as.Date("2009-01-01"), as.Date("2024-03-31"), by = "9 months"),
                   labels = date_format("%Y%m")) +  # Format as year-quarter
      
      theme(legend.position = c(0, 1),            # Position legend on the top-left corner
            legend.justification = c(0, 1),       # Anchor the legend to the top-left
            legend.text = element_text(size = 8, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
            axis.text.y = element_text(hjust = 1, face = "bold"),
            text = element_text(size = 10)) +
      
      theme(plot.title = element_text(size = 12, 
                                      hjust = 0.5,
                                      color = "black",
                                      face = "bold"))+
      
      theme(plot.subtitle = element_text(size = 10, 
                                         hjust = 0.5, 
                                         color = "darkgrey", 
                                         face = "bold"))
    
    plot(AU_fiscal_revenue_graph)
    
    #===========================================================================
    
    # Load necessary libraries
    library(ggplot2)
    library(dplyr)
    library(scales)  # For date_format
    
    # Filter the data for the required time period
    df_filtered <- AU_fiscal_sector %>%
      filter(TIME_PERIOD >= as.Date("2009-01-01") & TIME_PERIOD <= as.Date("2024-03-31"))
    
    # Build the stacked column chart
    AU_fiscal2gdp_graph <-
    ggplot(df_filtered, aes(x = TIME_PERIOD)) +
      geom_bar(aes(y = expense2gdp, fill = "Expense to GDP"), stat = "identity") +
      geom_bar(aes(y = revenue2gdp, fill = "Revenue to GDP"), stat = "identity") +
      geom_bar(aes(y = net_lending2gdp, fill = "Net Lending to GDP"), stat = "identity") +
      
      # Customize x-axis for quarterly data and remove padding from both axes
      scale_x_date(date_breaks = "3 months", date_labels = "%Y-Q%q", expand = c(0, 0)) +
      
      # Set titles and adjust hjust and vjust for centering and placement inside the plot area
      labs(title = "Government Fiscal Metrics (% of GDP)",
           subtitle = "In Percent of GDP, 2009Q1 - 2024Q1",
           x = NULL, 
           y = NULL) +
      
      # Adjust title and subtitle positions to fit inside the plot
      theme(plot.title = element_text(size = 15, 
                                      vjust = 3,        # Push the title inside the plot area
                                      hjust = 0.5,      # Center the title horizontally
                                      color = "black",
                                      face = "bold"),
            plot.subtitle = element_text(size = 10, 
                                         vjust = 2.5,      # Push the subtitle inside the plot area
                                         hjust = 0.5,    # Center the subtitle horizontally
                                         color = "darkgrey", 
                                         face = "bold")) +
      
      theme(panel.background=element_rect(fill="white"), 
            panel.grid.major.y = element_line(color="grey", linewidth = 0.2))+
      
      # Rotate x-axis labels for readability and adjust plot margins to fill the plot area
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.margin = margin(t = 20, r = 20, b = 20, l = 20)) +  # Adjust margins to fit the entire area
      
      # Customize legend and bar fill colors
      scale_fill_manual(name = "Legend", 
                        values = c("Expense to GDP" = "blue", 
                                   "Revenue to GDP" = "green", 
                                   "Net Lending to GDP" = "red")) +
      
      # Set legend position and direction to horizontal
      theme(legend.position = c(0.5, 0.075),        # Place legend at the bottom center
            legend.direction = "horizontal",       # Make the legend horizontal
            legend.title = element_text(face = "bold"),  # Bold legend title
            legend.text = element_text(size = 9)) +      # Adjust text size
      
      # Format x-axis with a 9-month interval and custom labels
      scale_x_date(breaks = seq(as.Date("2009-01-01"), as.Date("2024-03-31"), by = "9 months"),
                   labels = date_format("%Y%m"), expand = c(0, 0)) +  # Ensure the x-axis fills the entire space
      
      # Change Y-axis scale and remove space on the y-axis
      scale_y_continuous(limits = c(-20, 50), n.breaks = 10, expand = c(0, 0))  # Remove y-axis padding
    
    plot(AU_fiscal2gdp_graph)
    
    #===============================================================================  
    
    # Creating a graph of Australia's Public Debt:
    
    library(ggplot2)
    library(scales)
    library(tidyverse)
    library(dplyr)
    
    AU_public_debt_graph <- AU_public_debt %>%
      ggplot() +
      geom_line(aes(x = TIME_PERIOD, y = public_debt_bln_usd), 
                color = "red", 
                lwd = 1) +
      
      # Correct title and subtitle using ggtitle
      ggtitle("Australia's Public Debt", 
              subtitle = "In Million USD, 2017q1 - 2024q1") +
      
      # Axis labels (x and y)
      labs(x = NULL, y = NULL) +
      
      theme(plot.title = element_text(size = 15, 
                                      hjust = 0.5,
                                      color = "black",
                                      face = "bold")) +
      
      theme(plot.subtitle = element_text(size = 10, 
                                         hjust = 0.5, 
                                         color = "darkgrey", 
                                         face = "bold")) +
      
      theme(axis.title.x = element_text(size = 10, 
                                        color = "slateblue", 
                                        face = "bold")) +
      
      theme(axis.text.x = element_text(size = 10, 
                                       angle = -90,
                                       hjust = 1,
                                       face = "bold"))+
      
      theme(axis.text.y = element_text(size = 10,
                                        hjust=1,
                                        face = "bold"))+
      
      theme(axis.title.y = element_text(size = 10, 
                                        color = "slateblue", 
                                        hjust = 1, 
                                        face = "bold")) +
      
      theme(panel.background = element_rect(fill = "white"), 
            panel.grid.major.y = element_line(color = "grey", linewidth = 0.2)) +
      
      # Add axis lines and ticks
      theme(axis.line = element_line(color = "black"),
            axis.ticks = element_line(color = "black"),
            axis.ticks.length = unit(0.2, "cm")) +
      
      scale_x_date(limits = c(min(AU_public_debt$TIME_PERIOD), max(AU_public_debt$TIME_PERIOD)), 
                   breaks = "3 months", 
                   date_labels = "%Y-%m") +
    
      # Changing scale of Y-axis
      scale_y_continuous(limits = c(1200, 2200))
      
    plot(AU_public_debt_graph)
    
    
    #===============================================================================  
    
    # Creating a graph of Australia's Public Debt to GDP:
    
    library(ggplot2)
    library(scales)
    library(dplyr)
    
    # Creating a graph of Australia's Public Debt to GDP Ratio
    AU_public_debt2gdp_graph <- ggplot(AU_public_debt) +
      geom_line(aes(x = TIME_PERIOD, y = debt2gdp), 
                color = "red", 
                lwd = 1) +
      
      # Correct title and subtitle using ggtitle
      ggtitle("Australia's Public Debt to GDP Ratio", 
              subtitle = "In Percent, 2017q1 - 2024q1") +
      
      # Axis labels (x and y)
      labs(x = NULL, y = NULL) +
      
      theme(plot.title = element_text(size = 15, 
                                      hjust = 0.5,
                                      color = "black",
                                      face = "bold")) +
      
      theme(plot.subtitle = element_text(size = 10, 
                                         hjust = 0.5, 
                                         color = "darkgrey", 
                                         face = "bold")) +
      
      theme(axis.title.x = element_text(size = 10, 
                                        color = "slateblue", 
                                        face = "bold")) +
      
      theme(axis.text.x = element_text(size = 10, 
                                       angle = -90,
                                       hjust = 1,
                                       face = "bold")) +
      
      theme(axis.text.y = element_text(size = 10,
                                       hjust = 1,
                                       face = "bold")) +
      
      theme(axis.title.y = element_text(size = 10, 
                                        color = "slateblue", 
                                        hjust = 1, 
                                        face = "bold")) +
      
      theme(panel.background = element_rect(fill = "white"), 
            panel.grid.major.y = element_line(color = "grey", linewidth = 0.2)) +
      
      # Add axis lines and ticks
      theme(axis.line = element_line(color = "black"),
            axis.ticks = element_line(color = "black"),
            axis.ticks.length = unit(0.2, "cm")) +
      
      scale_x_date(limits = c(min(AU_public_debt$TIME_PERIOD), max(AU_public_debt$TIME_PERIOD)), 
                   date_breaks = "3 months", 
                   date_labels = "%Y-%m")+
      
      # Changing scale of Y-axis
      scale_y_continuous(limits = c(250, 450))
      
      # Display the graph
      AU_public_debt2gdp_graph
      
    #===========================================================================
    
    library(officer)
    
    # Save the graph as an image file (optional step if you prefer saving as PNG or JPEG)
    ggsave("inflation_graph.png", plot = AU_inflation_graph, width = 7, height = 5, dpi = 300)
    ggsave("AU_GDP_graph.png", plot = AU_GDP_graph, width = 7, height = 5, dpi = 300)
    
    # Create a new Word document
    macro_graph_doc <- officer::read_docx()
    
    # Add the title to the document
    macro_graph_doc <- macro_graph_doc %>% body_add_par("Australia's Macroeconomic Indicator Dashboard", style = "heading 1")
    
    # Add tile of [GDP graph] and the graph to the document (vector format for better resolution in Word)
    macro_graph_doc <- macro_graph_doc %>% body_add_par("Australia's GDP", style = "heading 2")
    macro_graph_doc <- macro_graph_doc %>% body_add_gg(value = AU_GDP_graph, width = 6, height = 4)
    
    # Add tile of [Real GDP Growth graph] to the document (vector format for better resolution in Word)
    macro_graph_doc <- macro_graph_doc %>% body_add_par("Australia's GDP Growth QoQ", style = "heading 2")
    macro_graph_doc <- macro_graph_doc %>% body_add_gg(value = AU_GDP_qoq_growth_graph, width = 6, height = 4)
    
    # Add tile of [Real GDP Growth graph] to the document (vector format for better resolution in Word)
    macro_graph_doc <- macro_graph_doc %>% body_add_par("Australia's GDP Growth YoY", style = "heading 2")
    macro_graph_doc <- macro_graph_doc %>% body_add_gg(value = AU_GDP_yoy_growth_graph, width = 6, height = 4)
    
    # Add tile of [Inflation graph] and the graph to the document (vector format for better resolution in Word)
    macro_graph_doc <- macro_graph_doc %>% body_add_par("Australia's Inflation Rate", style = "heading 2")
    macro_graph_doc <- macro_graph_doc %>% body_add_gg(value = AU_inflation_graph, width = 6, height = 4)
  
    # Add tile of [Fiscal Expenditure graph] and the graph to the document (vector format for better resolution in Word)
    macro_graph_doc <- macro_graph_doc %>% body_add_par("Australia's Government Expenditures by Type", style = "heading 2")
    macro_graph_doc <- macro_graph_doc %>% body_add_gg(value = AU_fiscal_expense_graph, width = 6, height = 4)
    
    # Add tile of [Fiscal Revenue graph] and the graph to the document (vector format for better resolution in Word)
    macro_graph_doc <- macro_graph_doc %>% body_add_par("Australia's Government Revenue by Type", style = "heading 2")
    macro_graph_doc <- macro_graph_doc %>% body_add_gg(value = AU_fiscal_revenue_graph, width = 6, height = 4)
    
    # Add tile of [Fiscal Balance to GDP graph] and the graph to the document (vector format for better resolution in Word)
    macro_graph_doc <- macro_graph_doc %>% body_add_par("Australia's Fiscal Metrics, % of GDP", style = "heading 2")
    macro_graph_doc <- macro_graph_doc %>% body_add_gg(value = AU_fiscal2gdp_graph, width = 6, height = 4)
    
    # Add tile of [General Government Debt Graph] and the graph to the document (vector format for better resolution in Word)
    macro_graph_doc <- macro_graph_doc %>% body_add_par("Australia's General Government Debt", style = "heading 2")
    macro_graph_doc <- macro_graph_doc %>% body_add_gg(value = AU_public_debt_graph, width = 6, height = 4)
    
    # Add tile of [General Government Debt to GDP ratio Graph] and the graph to the document (vector format for better resolution in Word)
    macro_graph_doc <- macro_graph_doc %>% body_add_par("Australia's Public Debt to GDP", style = "heading 2")
    macro_graph_doc <- macro_graph_doc %>% body_add_gg(value = AU_public_debt2gdp_graph, width = 6, height = 4)
    
    # Save the Word document
    print(macro_graph_doc, target = "AU_macro_report.docx")
    
    file.show("C:/Users/OUDOM/OneDrive/Documents/1. Personal/Study Rstudio/R files practiced/AU_macro_report.docx")
    
    
    #===========================
          
    