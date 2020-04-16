* Original storshare12 regions

set storshare12 'Regions' /
canada            
usa               
laca     
eastern_europe    
former_su         
middle_east      
india             
china             
australia_nz      
south_east_asia   
western_europe    
africa            
/;

$batinclude 'regions/iso3.inc'

set map_storshare12_iso3(storshare12,iso3) 'Mapping between storshare12 regions and countries/territories'/

canada.CAN # Canada
south_east_asia.JPN # Japan
australia_nz .NZL # New Zealand

china.CHN  # China
china.HKG  # Small countries: Honk Kong
china.MAC  #                  Macao
china.TWN  #                  Taiwan

south_east_asia.ASM  # East Asia
south_east_asia.BRN
south_east_asia.CCK
south_east_asia.FJI
south_east_asia.FSM
south_east_asia.GUM
south_east_asia.IDN
south_east_asia.KHM
south_east_asia.KIR
south_east_asia.PRK
south_east_asia.LAO
south_east_asia.MHL
south_east_asia.MMR
south_east_asia.MNG
south_east_asia.MNP
south_east_asia.MYS
south_east_asia.NCL
south_east_asia.PHL
south_east_asia.PLW
south_east_asia.PNG
south_east_asia.PYF
south_east_asia.SGP
south_east_asia.SLB
south_east_asia.THA
south_east_asia.TLS
south_east_asia.TON
south_east_asia.VNM
south_east_asia.VUT
south_east_asia.WSM

australia_nz .AUS # Australia
south_east_asia.KOR # South Korea
africa.ZAF # South Africa

laca.ABW
laca.AIA
laca.ARG
laca.ATG
laca.BES
laca.BHS
laca.BLM
laca.BLZ
laca.BOL
laca.BRA
laca.BRB
laca.CHL
laca.COL
laca.CRI
laca.CUB
laca.CUW
laca.CYM
laca.DMA
laca.DOM
laca.ECU
laca.FLK
laca.GLP
laca.GRD
laca.GTM
laca.GUF
laca.GUY
laca.HND
laca.HTI
laca.JAM
laca.KNA
laca.LCA
laca.MAF
laca.MEX
laca.MSR
laca.MTQ
laca.NIC
laca.PAN
laca.PER
laca.PRI
laca.PRY
laca.SLV
laca.SUR
laca.SXM
laca.TTO
laca.URY
laca.VCT
laca.VEN

india.IND # India

africa.DZA
africa.EGY
africa.ESH
africa.LBY
africa.MAR
africa.TUN

middle_east.ARE
middle_east.BHR
middle_east.DJI
middle_east.IRN
middle_east.IRQ
middle_east.ISR
middle_east.JOR
middle_east.KWT
middle_east.LBN
middle_east.OMN
middle_east.PSE
middle_east.QAT
middle_east.SAU
middle_east.SYR
middle_east.YEM

eastern_europe.BGR # EU countries (except EU-15+Malta)
eastern_europe.CYP #
eastern_europe.CZE #
eastern_europe.EST #
eastern_europe.KSV
eastern_europe.HUN #
eastern_europe.LTU #
eastern_europe.LVA #
eastern_europe.POL #
eastern_europe.ROU #
eastern_europe.SVK #
eastern_europe.SVN #
eastern_europe.MLT # Malta was formely in mena
eastern_europe.HRV # Croatia was formely in TE

western_europe.AUT # EU-15 Countries
western_europe.BEL #
western_europe.DEU #
western_europe.DNK #
western_europe.ESP #
western_europe.FIN #
western_europe.FRA #
western_europe.GBR #
western_europe.GRC #
western_europe.IRL #
western_europe.ITA #
western_europe.LUX #
western_europe.NLD #
western_europe.PRT #
western_europe.SWE #
western_europe.LIE # Liechtenstein
western_europe.NOR # Norway
western_europe.CHE # Switzerland
western_europe.GRL # Greenland (to Denmark)
western_europe.ISL # Iceland
western_europe.FRO # Island Feroe
western_europe.ALA # Small islands/countries
western_europe.AND #
western_europe.GGY #
western_europe.GIB #
western_europe.IMN #
western_europe.JEY #
western_europe.MCO #
western_europe.SJM #
western_europe.SMR #
western_europe.VAT #
western_europe.SPM # St Pierre et Miquelon (to France)

south_east_asia.AFG
south_east_asia.BGD
south_east_asia.BTN
south_east_asia.LKA
south_east_asia.MDV
south_east_asia.NPL
south_east_asia.PAK

africa.AGO
africa.BEN
africa.BWA
africa.BFA
africa.BDI
africa.CMR
africa.CPV
africa.CAF
africa.TCD
africa.COM
africa.COG
africa.COD
africa.CIV
africa.GNQ
africa.ERI
africa.ETH
africa.GAB
africa.GMB
africa.GHA
africa.GIN
africa.GNB
africa.KEN
africa.LSO
africa.LBR
africa.MDG
africa.MWI
africa.MLI
africa.MRT
africa.MUS
africa.MYT
africa.MOZ
africa.NAM
africa.NER
africa.NGA
africa.REU
africa.RWA
africa.STP
africa.SEN
africa.SYC
africa.SHN
africa.SLE
africa.SOM
africa.SSD
africa.SDN
africa.SWZ
africa.TZA
africa.TGO
africa.UGA
africa.ZMB
africa.ZWE

former_su.ALB
former_su.ARM
former_su.AZE
former_su.BLR
former_su.BIH
former_su.GEO
former_su.KAZ
former_su.KGZ
former_su.MKD
former_su.MDA
former_su.MNE
former_su.RUS
former_su.SRB
former_su.TJK
former_su.TUR
former_su.TKM
former_su.UKR
former_su.UZB

usa.USA

* other small territories
australia_nz .CXR # Christmas Island (to Australia)
laca.ATA # Antarctica
africa.ATF # French Southern Territories
laca.BMU # Bermuda
africa.BVT # Bouvet Island
australia_nz .COK # Cook Islands
australia_nz .HMD # Heard Island and McDonald Islands
africa.IOT # British Indian Ocean Territory
australia_nz .NFK # Norfolk Island
australia_nz .NIU # Niue
australia_nz .NRU # Nauru
australia_nz .PCN # Pitcairn
laca.SGS # South Georgia and the South Sandwich Islands
laca.TCA # Turks and Caicos Islands
australia_nz .TKL # Tokelau
australia_nz .TUV # Tuvalu
australia_nz .UMI # United States Minor Outlying Islands
laca.VGB # Virgin Islands, British
laca.VIR # Virgin Islands, U.S.
australia_nz .WLF # Wallis and Futuna

/;
