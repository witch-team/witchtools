* Original stordistance18 regions

set stordistance18 'Regions' /
canada
usa
central_am
south_am # including Brasil, Mexico, Caribbean Islands
north_afr
west_afr
east_afr
south_afr # both South Africa and southern Africa
west_eu
east_eu
former_su
middle_east
south_asia
east_asia
south_east_asia
oceania
japan
greenland
/;

$batinclude 'regions/iso3.inc'

set map_stordistance18_iso3(stordistance18,iso3) 'Mapping between stordistance18 regions and countries/territories'/

south_am.BES
south_am.ATA
south_am.BRA
canada.CAN # Canada
canada.SPM # St Pierre et Miquelon (to France)
east_eu.ALB
east_eu.BGR # EU countries (except EU-15+Malta)
east_eu.BIH
east_eu.CYP #
east_eu.CZE #
east_eu.KSV
former_su.EST #
east_eu.HRV
east_eu.HUN #
former_su.LTU #
former_su.LVA #
east_eu.MKD
west_eu.MLT
east_eu.MNE
east_eu.POL #
east_eu.ROU #
east_eu.SRB
east_eu.SVK #
east_eu.SVN #
east_asia.CHN  # China
east_asia.HKG  # Small countries: Honk Kong
east_asia.MAC  #                  Macao
east_asia.MNG  # Mongolia
east_asia.TWN  #                  Taiwan
east_afr.BDI
east_afr.DJI
east_afr.ERI
east_afr.ETH
east_afr.IOT # British Indian Ocean Territory
east_afr.KEN
east_afr.MDG
east_afr.MYS
east_afr.REU
east_afr.RWA
east_afr.SDN
east_afr.SOM
east_afr.SSD
east_afr.SYC
east_afr.TZA
east_afr.UGA
south_asia.IND # India
south_asia.IDN # Indonesia
south_asia.PNG
south_asia.TLS
japan.JPN # Japan
east_asia.KOR # South Korea
east_asia.PRK
middle_east.ARE
middle_east.BHR
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
south_am.MEX
north_afr.BMU # Bermuda
north_afr.DZA
north_afr.EGY
north_afr.ESH
north_afr.LBY
north_afr.MAR
north_afr.TUN
oceania.ASM
oceania.AUS # Australia
oceania.CCK
oceania.COK # Cook Islands
oceania.CXR # Christmas Island (to Australia)
oceania.FJI
oceania.FSM
oceania.GUM
oceania.HMD # Heard Island and McDonald Islands
oceania.KIR
oceania.MHL
oceania.MNP
oceania.NCL
oceania.NFK # Norfolk Island
oceania.NIU # Niue
oceania.NRU # Nauru
oceania.NZL # New Zealand
oceania.PCN # Pitcairn
oceania.PYF
oceania.SLB
oceania.TCA # Turks and Caicos Islands
oceania.TKL # Tokelau
oceania.TON
oceania.TUV # Tuvalu
oceania.UMI # United States Minor Outlying Islands
oceania.VUT
oceania.WLF # Wallis and Futuna
oceania.WSM
central_am.ABW
central_am.AIA
central_am.ATG
central_am.BHS
central_am.BLM
central_am.BLZ
central_am.BRB
central_am.CRI
central_am.CUB
central_am.CUW
central_am.CYM
central_am.DMA
central_am.DOM
central_am.FLK
central_am.GLP
central_am.GRD
central_am.GTM
central_am.HND
central_am.HTI
central_am.JAM
central_am.KNA
central_am.LCA
central_am.MAF
central_am.MSR
central_am.MTQ
central_am.NIC
central_am.PAN
central_am.PRI
central_am.SGS # South Georgia and the South Sandwich Islands
central_am.SLV
central_am.SXM
central_am.TTO
central_am.VCT
central_am.VGB # Virgin Islands, British
central_am.VIR # Virgin Islands, U.S.
south_afr.AGO
south_afr.BWA
south_afr.COM
south_afr.LSO
south_afr.MOZ
south_afr.MWI
south_afr.MYT
south_afr.NAM
south_afr.SWZ
south_afr.ZMB
south_afr.ZWE
south_am.ARG
south_am.BOL
south_am.CHL
south_am.COL
south_am.ECU
south_am.GUF
south_am.GUY
south_am.PER
south_am.PRY
south_am.SUR
south_am.URY
south_am.VEN
south_asia.AFG
south_asia.NPL
south_asia.PAK
former_su.ARM
former_su.AZE
former_su.GEO
former_su.RUS
south_afr.ATF # French Southern Territories
south_afr.BVT # Bouvet Island
south_afr.ZAF # South Africa
south_east_asia.BGD
south_east_asia.BRN
south_east_asia.BTN
south_east_asia.KHM
south_east_asia.LAO
south_east_asia.LKA
south_east_asia.MDV
south_east_asia.MMR
south_east_asia.PHL
south_east_asia.PLW
south_east_asia.SGP
south_east_asia.THA
south_east_asia.VNM
former_su.KAZ
former_su.KGZ
former_su.TJK
former_su.TKM
former_su.UZB
middle_east.TUR
former_su.BLR
former_su.MDA
former_su.UKR
usa.USA
west_afr.BEN
west_afr.BFA
west_afr.CAF
west_afr.CIV
west_afr.CMR
west_afr.COD
west_afr.COG
west_afr.CPV
west_afr.GAB
west_afr.GHA
west_afr.GIN
west_afr.GMB
west_afr.GNB
west_afr.GNQ
west_afr.LBR
west_afr.MLI
west_afr.MRT
west_afr.MUS
west_afr.NER
west_afr.NGA
west_afr.SEN
west_afr.SHN
west_afr.SLE
west_afr.STP
west_afr.TCD
west_afr.TGO
west_eu.ALA # Small islands countries
west_eu.AND #
west_eu.AUT # EU-15 Countries
west_eu.BEL #
west_eu.CHE # Switzerland
west_eu.DEU #
west_eu.DNK #
west_eu.ESP #
west_eu.FIN #
west_eu.FRA #
west_eu.FRO # Island Feroe
west_eu.GBR #
west_eu.GGY #
west_eu.GIB #
east_eu.GRC #
greenland.GRL
west_eu.IMN #
west_eu.IRL #
west_eu.ISL # Iceland
west_eu.ITA #
west_eu.JEY #
west_eu.LIE # Liechtenstein
west_eu.LUX #
west_eu.MCO #
west_eu.NLD #
west_eu.NOR # Norway
west_eu.PRT #
west_eu.SJM #
west_eu.SMR #
west_eu.SWE #
west_eu.VAT #

/;


