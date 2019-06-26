* Original storcapeor9 regions

set storcapeor9 'Regions' /
canada
asia
laca
mena
europe
sasia
ssa
te
usa
others
/;

$batinclude 'regions/iso3.inc'

set map_storcapeor9_iso3(storcapeor9,iso3) 'Mapping between storcapeor9 regions and countries/territories'/

canada.CAN # Canada
asia.JPN # Japan
others.NZL # New Zealand

asia.CHN  # China
asia.HKG  # Small countries: Honk Kong
asia.MAC  #                  Macao
asia.TWN  #                  Taiwan

asia.ASM  # East Asia
asia.BRN
asia.CCK
asia.FJI
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
asia.PNG
asia.PYF
asia.SGP
asia.SLB
asia.THA
asia.TLS
asia.TON
asia.VNM
asia.VUT
asia.WSM

others.AUS # Australia
asia.KOR # South Korea
ssa.ZAF # South Africa

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

asia.IND # India

mena.ARE
mena.BHR
mena.DJI
mena.DZA
mena.EGY
mena.ESH
mena.IRN
mena.IRQ
mena.ISR
mena.JOR
mena.KWT
mena.LBN
mena.LBY
mena.MAR
mena.OMN
mena.PSE
mena.QAT
mena.SAU
mena.SYR
mena.TUN
mena.YEM

europe.BGR # EU countries (except EU-15+Malta)
europe.CYP #
europe.CZE #
europe.EST #
europe.HUN #
europe.LTU #
europe.LVA #
europe.POL #
europe.ROU #
europe.SVK #
europe.SVN #
europe.MLT # Malta was formely in mena
europe.HRV # Croatia was formely in TE

europe.AUT # EU-15 Countries
europe.BEL #
europe.DEU #
europe.DNK #
europe.ESP #
europe.FIN #
europe.FRA #
europe.GBR #
europe.GRC #
europe.KSV
europe.IRL #
europe.ITA #
europe.LUX #
europe.NLD #
europe.PRT #
europe.SWE #
europe.LIE # Liechtenstein
europe.NOR # Norway
europe.CHE # Switzerland
europe.GRL # Greenland (to Denmark)
europe.ISL # Iceland
europe.FRO # Island Feroe
europe.ALA # Small islands/countries
europe.AND #
europe.GGY #
europe.GIB #
europe.IMN #
europe.JEY #
europe.MCO #
europe.SJM #
europe.SMR #
europe.VAT #
europe.SPM # St Pierre et Miquelon (to France)

sasia.AFG
sasia.BGD
sasia.BTN
sasia.LKA
sasia.MDV
sasia.NPL
sasia.PAK

ssa.AGO
ssa.BEN
ssa.BWA
ssa.BFA
ssa.BDI
ssa.CMR
ssa.CPV
ssa.CAF
ssa.TCD
ssa.COM
ssa.COG
ssa.COD
ssa.CIV
ssa.GNQ
ssa.ERI
ssa.ETH
ssa.GAB
ssa.GMB
ssa.GHA
ssa.GIN
ssa.GNB
ssa.KEN
ssa.LSO
ssa.LBR
ssa.MDG
ssa.MWI
ssa.MLI
ssa.MRT
ssa.MUS
ssa.MYT
ssa.MOZ
ssa.NAM
ssa.NER
ssa.NGA
ssa.REU
ssa.RWA
ssa.STP
ssa.SEN
ssa.SYC
ssa.SHN
ssa.SLE
ssa.SOM
ssa.SSD
ssa.SDN
ssa.SWZ
ssa.TZA
ssa.TGO
ssa.UGA
ssa.ZMB
ssa.ZWE

te.ALB
te.ARM
te.AZE
te.BLR
te.BIH
te.GEO
te.KAZ
te.KGZ
te.MKD
te.MDA
te.MNE
te.RUS
te.SRB
te.TJK
te.TUR
te.TKM
te.UKR
te.UZB

usa.USA

* other small territories
others.CXR # Christmas Island (to Australia)
laca.ATA # Antarctica
ssa.ATF # French Southern Territories
laca.BMU # Bermuda
ssa.BVT # Bouvet Island
canada.COK # Cook Islands
others.HMD # Heard Island and McDonald Islands
ssa.IOT # British Indian Ocean Territory
others.NFK # Norfolk Island
canada.NIU # Niue
others.NRU # Nauru
canada.PCN # Pitcairn
laca.SGS # South Georgia and the South Sandwich Islands
laca.TCA # Turks and Caicos Islands
canada.TKL # Tokelau
canada.TUV # Tuvalu
canada.UMI # United States Minor Outlying Islands
laca.VGB # Virgin Islands, British
laca.VIR # Virgin Islands, U.S.
canada.WLF # Wallis and Futuna

/;
