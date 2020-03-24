* storcapieaghg7 regions

set storcapieaghg7 'Regions' /
africa_me              # Africa and Middle East
asia                   # Asia
oceania                # Oceania
latin_america          # Central and South America
non_oecd_former_soviet # East Europe, Russia and remaining countries
north_america          # Canada and US
oecd_eu                # OECD Europe
/;

$batinclude 'regions/iso3.inc'

set map_storcapieaghg7_iso3(storcapieaghg7,iso3) 'Mapping between storcapieaghg7 regions and countries/territories'/

africa_me.AGO
africa_me.BEN
africa_me.BWA
africa_me.BFA
africa_me.BDI
africa_me.CMR
africa_me.CPV
africa_me.CAF
africa_me.TCD
africa_me.COM
africa_me.COG
africa_me.COD
africa_me.CIV
africa_me.GNQ
africa_me.ERI
africa_me.ETH
africa_me.GAB
africa_me.GMB
africa_me.GHA
africa_me.GIN
africa_me.GNB
africa_me.KEN
africa_me.LSO
africa_me.LBR
africa_me.MDG
africa_me.MWI
africa_me.MLI
africa_me.MRT
africa_me.MUS
africa_me.MYT
africa_me.MOZ
africa_me.NAM
africa_me.NER
africa_me.NGA
africa_me.REU
africa_me.RWA
africa_me.STP
africa_me.SEN
africa_me.SYC
africa_me.SHN
africa_me.SLE
africa_me.SOM
africa_me.SSD
africa_me.SDN
africa_me.SWZ
africa_me.TZA
africa_me.TGO
africa_me.UGA
africa_me.ZMB
africa_me.ZWE
africa_me.ZAF # South Africa
africa_me.ARE # Mena
africa_me.BHR
africa_me.DJI
africa_me.DZA
africa_me.EGY
africa_me.ESH
africa_me.IRN
africa_me.IRQ
africa_me.ISR
africa_me.JOR
africa_me.KWT
africa_me.LBN
africa_me.LBY
africa_me.MAR
africa_me.OMN
africa_me.PSE
africa_me.QAT
africa_me.SAU
africa_me.SYR
africa_me.TUN
africa_me.YEM

asia.JPN # Japan
asia.CHN  # China
asia.HKG  # Small countries: Honk Kong
asia.MAC  #                  Macao
asia.TWN  #                  Taiwan
asia.ASM  # East Asia
asia.BRN
asia.CCK
asia.FSM
asia.GUM
asia.IDN
asia.KHM
asia.KIR
asia.PRK
asia.LAO
asia.MHL
asia.MMR
asia.MNG
asia.MNP
asia.MYS
asia.NCL
asia.PHL
asia.PLW
asia.PYF
asia.SGP
asia.SLB
asia.THA
asia.TLS
asia.TON
asia.VNM
asia.VUT
asia.WSM
asia.AFG # South Asia
asia.BGD
asia.BTN
asia.LKA
asia.MDV
asia.NPL
asia.PAK
asia.KOR # South Korea
asia.IND # India

oceania.AUS # Australia
oceania.NZL # New Zealand
oceania.FJI
oceania.PNG

latin_america.ABW
latin_america.AIA
latin_america.ARG
latin_america.ATG
latin_america.BES
latin_america.BHS
latin_america.BLM
latin_america.BLZ
latin_america.BOL
latin_america.BRA
latin_america.BRB
latin_america.CHL
latin_america.COL
latin_america.CRI
latin_america.CUB
latin_america.CUW
latin_america.CYM
latin_america.DMA
latin_america.DOM
latin_america.ECU
latin_america.FLK
latin_america.GLP
latin_america.GRD
latin_america.GTM
latin_america.GUF
latin_america.GUY
latin_america.HND
latin_america.HTI
latin_america.JAM
latin_america.KNA
latin_america.LCA
latin_america.MAF
latin_america.MEX
latin_america.MSR
latin_america.MTQ
latin_america.NIC
latin_america.PAN
latin_america.PER
latin_america.PRI
latin_america.PRY
latin_america.SLV
latin_america.SUR
latin_america.SXM
latin_america.TTO
latin_america.URY
latin_america.VCT
latin_america.VEN


oecd_eu.CZE #
oecd_eu.EST #
oecd_eu.HUN #
oecd_eu.LVA #
oecd_eu.POL #
oecd_eu.SVK #
oecd_eu.SVN #


oecd_eu.ALA # Small islands/countries
oecd_eu.AND #
oecd_eu.AUT # EU-15 Countries
oecd_eu.BEL #
oecd_eu.CHE # Switzerland
oecd_eu.DEU #
oecd_eu.DNK #
oecd_eu.ESP #
oecd_eu.FIN #
oecd_eu.FRA #
oecd_eu.FRO # Island Feroe
oecd_eu.GBR #
oecd_eu.GGY #
oecd_eu.GIB #
oecd_eu.GRC #
oecd_eu.GRL # Greenland (to Denmark)
oecd_eu.IMN #
oecd_eu.IRL #
oecd_eu.ISL # Iceland
oecd_eu.ITA #
oecd_eu.JEY #
oecd_eu.LUX #
oecd_eu.MCO #
oecd_eu.NLD #
oecd_eu.NOR # Norway
oecd_eu.PRT #
oecd_eu.SJM #
oecd_eu.SMR #
oecd_eu.SPM # St Pierre et Miquelon (to France)
oecd_eu.SWE #
oecd_eu.VAT #

non_oecd_former_soviet.ALB
non_oecd_former_soviet.ARM
non_oecd_former_soviet.AZE
non_oecd_former_soviet.BGR # EU countries (except EU-15+Malta)
non_oecd_former_soviet.BIH
non_oecd_former_soviet.BLR
non_oecd_former_soviet.CYP #
non_oecd_former_soviet.GEO
non_oecd_former_soviet.HRV # Croatia was formely in TE
non_oecd_former_soviet.KAZ
non_oecd_former_soviet.KGZ
non_oecd_former_soviet.KSV
non_oecd_former_soviet.LIE # Liechtenstein
non_oecd_former_soviet.LTU #
non_oecd_former_soviet.MDA
non_oecd_former_soviet.MKD
non_oecd_former_soviet.MLT # Malta was formely in africa_me
non_oecd_former_soviet.MNE
non_oecd_former_soviet.ROU #
non_oecd_former_soviet.RUS
non_oecd_former_soviet.SRB
non_oecd_former_soviet.TJK
non_oecd_former_soviet.TKM
non_oecd_former_soviet.TUR
non_oecd_former_soviet.UKR
non_oecd_former_soviet.UZB

north_america.CAN # Canada
north_america.USA

* other small territories
oceania.CXR # Christmas Island (to Australia)
latin_america.ATA # Antarctica
africa_me.ATF # French Southern Territories
latin_america.BMU # Bermuda
africa_me.BVT # Bouvet Island
oceania.COK # Cook Islands
oceania.HMD # Heard Island and McDonald Islands
africa_me.IOT # British Indian Ocean Territory
oceania.NFK # Norfolk Island
oceania.NIU # Niue
oceania.NRU # Nauru
oceania.PCN # Pitcairn
latin_america.SGS # South Georgia and the South Sandwich Islands
latin_america.TCA # Turks and Caicos Islands
oceania.TKL # Tokelau
oceania.TUV # Tuvalu
oceania.UMI # United States Minor Outlying Islands
latin_america.VGB # Virgin Islands, British
latin_america.VIR # Virgin Islands, U.S.
oceania.WLF # Wallis and Futuna

/;
