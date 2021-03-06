* IEA WEO WEM model regions

set iea 'Regions' /
NAM #subtract US
US
CSAM #subtrat Brazil
BRAZIL
EUR
Africa #subtract SAFR
SAFR
ME
EURASIA #subtract RUS
RUS
ASIAPAC #subtract CHINA, INDIA, JPN
CHINA
INDIA
JPN
/;

$batinclude 'regions/iso3.inc'

set map_iea_iso3(iea,iso3) 'Mapping between R5 regions and countries'/

NAM.CAN # Canada
JPN.JPN # Japan
ASIAPAC.NZL # New Zealand

CHINA.CHN  # ASIAPAC
CHINA.HKG  # Small countries: Honk Kong
CHINA.MAC  #                  Macao
CHINA.TWN  #                  Taiwan

ASIAPAC.ASM  # East Asia
ASIAPAC.BRN
ASIAPAC.CCK
ASIAPAC.FJI
ASIAPAC.FSM
ASIAPAC.GUM
ASIAPAC.IDN
ASIAPAC.KHM
ASIAPAC.KIR
ASIAPAC.PRK
ASIAPAC.LAO
ASIAPAC.MHL
ASIAPAC.MMR
ASIAPAC.MNG
ASIAPAC.MNP
ASIAPAC.MYS
ASIAPAC.NCL
ASIAPAC.PHL
ASIAPAC.PLW
ASIAPAC.PNG
ASIAPAC.PYF
ASIAPAC.SGP
ASIAPAC.SLB
ASIAPAC.THA
ASIAPAC.TLS
ASIAPAC.TON
ASIAPAC.VNM
ASIAPAC.VUT
ASIAPAC.WSM

ASIAPAC.AUS # Australia
ASIAPAC.KOR # South Korea
SAFR.ZAF # South Africa

CSAM.ABW
CSAM.AIA
CSAM.ARG
CSAM.ATG
CSAM.BES
CSAM.BHS
CSAM.BLM
CSAM.BLZ
CSAM.BOL
BRAZIL.BRA
CSAM.BRB
CSAM.CHL
CSAM.COL
CSAM.CRI
CSAM.CUB
CSAM.CUW
CSAM.CYM
CSAM.DMA
CSAM.DOM
CSAM.ECU
CSAM.FLK
CSAM.GLP
CSAM.GRD
CSAM.GTM
CSAM.GUF
CSAM.GUY
CSAM.HND
CSAM.HTI
CSAM.JAM
CSAM.KNA
CSAM.LCA
CSAM.MAF
NAM.MEX
CSAM.MSR
CSAM.MTQ
CSAM.NIC
CSAM.PAN
CSAM.PER
CSAM.PRI
CSAM.PRY
CSAM.SLV
CSAM.SUR
CSAM.SXM
CSAM.TTO
CSAM.URY
CSAM.VCT
CSAM.VEN

India.IND # ASIAPAC

ME.ARE
ME.BHR
ME.IRN
ME.IRQ
ME.JOR
ME.KWT
ME.LBN
ME.OMN
ME.QAT
ME.SAU
ME.SYR
ME.YEM

EUR.ISR
EUR.LBY

Africa.MAR
Africa.TUN
Africa.EGY
Africa.DZA
Africa.DJI
Africa.ESH
Africa.PSE

EUR.BGR # EU countries (except EU-15+Malta)
EUR.CYP #
EUR.CZE #
EUR.EST #
EUR.KSV
EUR.HUN #
EUR.LTU #
EUR.LVA #
EUR.POL #
EUR.ROU #
EUR.SVK #
EUR.SVN #
EUR.MLT # Malta was formely in R5MAF
EUR.HRV # Croatia was formely in R5REF

EUR.AUT # EU-15 Countries
EUR.BEL #
EUR.DEU #
EUR.DNK #
EUR.ESP #
EUR.FIN #
EUR.FRA #
EUR.GBR #
EUR.GRC #
EUR.IRL #
EUR.ITA #
EUR.LUX #
EUR.NLD #
EUR.PRT #
EUR.SWE #
EUR.LIE # Liechenstein
EUR.NOR # Norway
EUR.CHE # Switzerland
EUR.GRL # Greenland (to Denmark)
EUR.ISL # Iceland
EUR.FRO # Island Feroe
EUR.ALA # Small islands/countries
EUR.AND #
EUR.GGY #
EUR.GIB #
EUR.IMN #
EUR.JEY #
EUR.MCO #
EUR.SJM #
EUR.SMR #
EUR.VAT #
EUR.SPM # St Pierre et Miquelon (to France)

ASIAPAC.AFG
ASIAPAC.BGD
ASIAPAC.BTN
ASIAPAC.LKA
ASIAPAC.MDV
ASIAPAC.NPL
ASIAPAC.PAK

Africa.AGO
Africa.BEN
Africa.BWA
Africa.BFA
Africa.BDI
Africa.CMR
Africa.CPV
Africa.CAF
Africa.TCD
Africa.COM
Africa.COG
Africa.COD
Africa.CIV
Africa.GNQ
Africa.ERI
Africa.ETH
Africa.GAB
Africa.GMB
Africa.GHA
Africa.GIN
Africa.GNB
Africa.KEN
Africa.LSO
Africa.LBR
Africa.MDG
Africa.MWI
Africa.MLI
Africa.MRT
Africa.MUS
Africa.MYT
Africa.MOZ
Africa.NAM
Africa.NER
Africa.NGA
Africa.REU
Africa.RWA
Africa.STP
Africa.SEN
Africa.SYC
Africa.SHN
Africa.SLE
Africa.SOM
Africa.SSD
Africa.SDN
Africa.SWZ
Africa.TZA
Africa.TGO
Africa.UGA
Africa.ZMB
Africa.ZWE

EURASIA.ALB
EURASIA.ARM
EURASIA.AZE
EURASIA.BLR
EURASIA.BIH
EURASIA.GEO
EURASIA.KAZ
EURASIA.KGZ
EURASIA.MKD
EURASIA.MDA
EURASIA.MNE
RUS.RUS
EURASIA.SRB
EURASIA.TJK
EURASIA.TUR
EURASIA.TKM
EURASIA.UKR
EURASIA.UZB

US.USA

* other small Territories
ASIAPAC.CXR # Christmas Island (to Australia)
ASIAPAC.ATA # Antarctica
ASIAPAC.ATF # French Southern Territories
CSAM.BMU # Bermuda
ASIAPAC.BVT # Bouvet Island
ASIAPAC.COK # Cook Islands
ASIAPAC.HMD # Heard Island and McDonald Islands
ASIAPAC.IOT # British Indian Ocean Territory
ASIAPAC.NFK # Norfolk Island
ASIAPAC.NIU # Niue
ASIAPAC.NRU # Nauru
ASIAPAC.PCN # Pitcairn
CSAM.SGS # South Georgia and the South Sandwich Islands
CSAM.TCA # Turks and Caicos Islands
ASIAPAC.TKL # Tokelau
ASIAPAC.TUV # Tuvalu
ASIAPAC.UMI # United States Minor Outlying Islands
CSAM.VGB # Virgin Islands, British
CSAM.VIR # Virgin Islands, U.S.
ASIAPAC.WLF # Wallis and Futuna

/;


