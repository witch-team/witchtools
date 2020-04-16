* Kyle GCAM 14 regions

set kyle14 'Regions' /
us                       # United States of America
canada                    # Canada
western_europe            # Western Europe (EU15+EFTA)
eastern_europe            # Eastern Europe (EU12+European EITs excluding FSU countries)
japan                     # Japan
australia_nz              # Australia & New Zeland
former_soviet_union       # Former Soviet Union
china                     # China
middle_east               # Middle East
africa                    # Africa
latin_america             # Latin America, Mexico and Caribbean
southeast_asia            # South East Asia 
korea                     # Korea
india                     # India
/;

$batinclude 'regions/iso3.inc'

set map_kyle14_iso3(kyle14,iso3) 'Mapping between WITCH regions and countries/territories'/

US.USA     # USA

Canada.CAN  # Canada

Western_Europe.AUT # EU-15 Countries, for the moment western europe is assumed to be the same as our oldeuro, still to be checked
Western_Europe.BEL #
Western_Europe.DEU #
Western_Europe.DNK #
Western_Europe.ESP #
Western_Europe.FIN #
Western_Europe.FRA #
Western_Europe.GBR #
Western_Europe.GRC #
Western_Europe.IRL #
Western_Europe.ITA #
Western_Europe.LUX #
Western_Europe.NLD #
Western_Europe.PRT #
Western_Europe.SWE #
Western_Europe.LIE # Liechtenstein
Western_Europe.NOR # Norway
Western_Europe.CHE # Switzerland
Western_Europe.GRL # Greenland (to Denmark)
Western_Europe.ISL # Iceland
Western_Europe.FRO # Island Feroe
Western_Europe.ALA # Small islands/countries
Western_Europe.AND #
Western_Europe.GGY #
Western_Europe.GIB #
Western_Europe.IMN #
Western_Europe.JEY #
Western_Europe.MCO #
Western_Europe.SJM #
Western_Europe.SMR #
Western_Europe.VAT #
Western_Europe.SPM # St Pierre et Miquelon (to France)

Eastern_Europe.BGR # EU countries (except EU-15+Malta), for the moment eastern europe is assumed to be the same as our neweuro, still to be checked
Eastern_Europe.CYP #
Eastern_Europe.CZE #
Eastern_Europe.EST #
eastern_europe.KSV
Eastern_Europe.HUN #
Eastern_Europe.LTU #
Eastern_Europe.LVA #
Eastern_Europe.POL #
Eastern_Europe.ROU #
Eastern_Europe.SVK #
Eastern_Europe.SVN #
Eastern_Europe.MLT # Malta was formely in mena
Eastern_Europe.HRV # Croatia was formely in TE

Japan.JPN # Japan

Australia_NZ.AUS # Australia
Australia_NZ.NZL # New Zealand

Former_Soviet_Union.ALB # for the moment Former Soviet Union is assumed to be the same as our te, still to be checked
Former_Soviet_Union.ARM #
Former_Soviet_Union.AZE #
Former_Soviet_Union.BLR #
Former_Soviet_Union.BIH #
Former_Soviet_Union.GEO #
Former_Soviet_Union.KAZ #
Former_Soviet_Union.KGZ #
Former_Soviet_Union.MKD #
Former_Soviet_Union.MDA #
Former_Soviet_Union.MNE #
Former_Soviet_Union.RUS #
Former_Soviet_Union.SRB #
Former_Soviet_Union.TJK #
Former_Soviet_Union.TUR #
Former_Soviet_Union.TKM #
Former_Soviet_Union.UKR #
Former_Soviet_Union.UZB #

China.CHN  # China
China.HKG  # Small countries: Honk Kong
China.MAC  # Macao
China.TWN  # Taiwan

Middle_East.ARE # Middle East countries (obtained from mena separating north africa)
Middle_East.BHR # 
Middle_East.DJI # 
Middle_East.ESH # 
Middle_East.IRN # 
Middle_East.IRQ # 
Middle_East.ISR # 
Middle_East.JOR # 
Middle_East.KWT # 
Middle_East.LBN # 
Middle_East.OMN # 
Middle_East.PSE # 
Middle_East.QAT # 
Middle_East.SAU # 
Middle_East.SYR # 
Middle_East.YEM # 

Africa.DZA # North Africa
Africa.MAR # North Africa
Africa.LBY # North Africa
Africa.EGY # North Africa
Africa.TUN # North Africa
Africa.ZAF # South Africa
Africa.AGO # ssa
Africa.BEN # ssa
Africa.BWA # ssa
Africa.BFA # ssa
Africa.BDI # ssa
Africa.CMR # ssa
Africa.CPV # ssa
Africa.CAF # ssa
Africa.TCD # ssa
Africa.COM # ssa
Africa.COG # ssa
Africa.COD # ssa
Africa.CIV # ssa
Africa.GNQ # ssa
Africa.ERI # ssa
Africa.ETH # ssa
Africa.GAB # ssa
Africa.GMB # ssa
Africa.GHA # ssa
Africa.GIN # ssa
Africa.GNB # ssa
Africa.KEN # ssa
Africa.LSO # ssa
Africa.LBR # ssa
Africa.MDG # ssa
Africa.MWI # ssa
Africa.MLI # ssa
Africa.MRT # ssa
Africa.MUS # ssa
Africa.MYT # ssa
Africa.MOZ # ssa
Africa.NAM # ssa
Africa.NER # ssa
Africa.NGA # ssa
Africa.REU # ssa
Africa.RWA # ssa
Africa.STP # ssa
Africa.SEN # ssa
Africa.SYC # ssa
Africa.SHN # ssa
Africa.SLE # ssa
Africa.SOM # ssa
Africa.SSD # ssa
Africa.SDN # ssa
Africa.SWZ # ssa
Africa.TZA # ssa
Africa.TGO # ssa
Africa.UGA # ssa
Africa.ZMB # ssa
Africa.ZWE # ssa

Latin_America.ABW
Latin_America.AIA
Latin_America.ARG
Latin_America.ATG
Latin_America.BES
Latin_America.BHS
Latin_America.BLM
Latin_America.BLZ
Latin_America.BOL
Latin_America.BRA
Latin_America.BRB
Latin_America.CHL
Latin_America.COL
Latin_America.CRI
Latin_America.CUB
Latin_America.CUW
Latin_America.CYM
Latin_America.DMA
Latin_America.DOM
Latin_America.ECU
Latin_America.FLK
Latin_America.GLP
Latin_America.GRD
Latin_America.GTM
Latin_America.GUF
Latin_America.GUY
Latin_America.HND
Latin_America.HTI
Latin_America.JAM
Latin_America.KNA
Latin_America.LCA
Latin_America.MAF
Latin_America.MEX
Latin_America.MSR
Latin_America.MTQ
Latin_America.NIC
Latin_America.PAN
Latin_America.PER
Latin_America.PRI
Latin_America.PRY
Latin_America.SLV
Latin_America.SUR
Latin_America.SXM
Latin_America.TTO
Latin_America.URY
Latin_America.VCT
Latin_America.VEN

Southeast_Asia.ASM # easia
Southeast_Asia.BRN # easia
Southeast_Asia.CCK # easia
Southeast_Asia.FJI # easia
Southeast_Asia.FSM # easia
Southeast_Asia.GUM # easia
Southeast_Asia.IDN # easia
Southeast_Asia.KHM # easia
Southeast_Asia.KIR # easia
Southeast_Asia.PRK # easia
Southeast_Asia.LAO # easia
Southeast_Asia.MHL # easia
Southeast_Asia.MMR # easia
Southeast_Asia.MNG # easia
Southeast_Asia.MNP # easia
Southeast_Asia.MYS # easia
Southeast_Asia.NCL # easia
Southeast_Asia.PHL # easia
Southeast_Asia.PLW # easia
Southeast_Asia.PNG # easia
Southeast_Asia.PYF # easia
Southeast_Asia.SGP # easia
Southeast_Asia.SLB # easia
Southeast_Asia.THA # easia
Southeast_Asia.TLS # easia
Southeast_Asia.TON # easia
Southeast_Asia.VNM # easia
Southeast_Asia.VUT # easia
Southeast_Asia.WSM # easia
Southeast_Asia.AFG # sasia
Southeast_Asia.BGD # sasia
Southeast_Asia.BTN # sasia
Southeast_Asia.LKA # sasia
Southeast_Asia.MDV # sasia
Southeast_Asia.NPL # sasia
Southeast_Asia.PAK # sasia

Korea.KOR # South Korea

India.IND # India


* other small territories
Australia_NZ.CXR   # Christmas Island (to Australia)
Latin_America.ATA  # Antarctica
Africa.ATF         # French Southern Territories
Latin_America.BMU  # Bermuda
Africa.BVT         # Bouvet Island
Australia_NZ.COK   # Cook Islands
Australia_NZ.HMD   # Heard Island and McDonald Islands
Africa.IOT         # British Indian Ocean Territory
Australia_NZ.NFK   # Norfolk Island
Australia_NZ.NIU   # Niue
Australia_NZ.NRU   # Nauru
Australia_NZ.PCN   # Pitcairn
Latin_America.SGS  # South Georgia and the South Sandwich Islands
Latin_America.TCA  # Turks and Caicos Islands
Australia_NZ.TKL   # Tokelau
Australia_NZ.TUV   # Tuvalu
Australia_NZ.UMI   # United States Minor Outlying Islands
Latin_America.VGB  # Virgin Islands, British
Latin_America.VIR  # Virgin Islands, U.S.
Australia_NZ.WLF   # Wallis and Futuna

/;


