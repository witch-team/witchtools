* Original WITCH 13 regions

set witch13 'Regions' /
usa      # United States of America
oldeuro  # Western Europe (EU15+EFTA)
neweuro  # Eastern Europe (EU12+European EITs excluding FSU countries)
kosau    # South Korea, South Africa, Australia
cajaz    # Canada, Japan, New Zeland
te       # Non-EU Eastern European countries, including Russia
mena     # Middle East and North Africa
ssa      # Sub Saharan Africa
sasia    # South Asia (except India)
china    # China, including Taiwan
easia    # South East Asia, including Indonesia
laca     # Latin America, Mexico and Caribbean
india    # India
/;

$batinclude 'regions/iso3.inc'

set map_witch13_iso3(witch13,iso3) 'Mapping between WITCH regions and countries/territories'/

cajaz.CAN # Canada
cajaz.JPN # Japan
cajaz.NZL # New Zealand

china.CHN  # China
china.HKG  # Small countries: Honk Kong
china.MAC  #                  Macao
china.TWN  #                  Taiwan

easia.ASM  # East Asia
easia.BRN
easia.CCK
easia.FJI
easia.FSM
easia.GUM
easia.IDN
easia.KHM
easia.KIR
easia.PRK
easia.LAO
easia.MHL
easia.MMR
easia.MNG
easia.MNP
easia.MYS
easia.NCL
easia.PHL
easia.PLW
easia.PNG
easia.PYF
easia.SGP
easia.SLB
easia.THA
easia.TLS
easia.TON
easia.VNM
easia.VUT
easia.WSM

kosau.AUS # Australia
kosau.KOR # South Korea
kosau.ZAF # South Africa

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

neweuro.BGR # EU countries (except EU-15+Malta)
neweuro.CYP #
neweuro.CZE #
neweuro.EST #
neweuro.HUN #
neweuro.LTU #
neweuro.LVA #
neweuro.POL #
neweuro.ROU #
neweuro.SVK #
neweuro.SVN #
neweuro.MLT # Malta was formely in mena
neweuro.HRV # Croatia was formely in TE

oldeuro.AUT # EU-15 Countries
oldeuro.BEL #
oldeuro.DEU #
oldeuro.DNK #
oldeuro.ESP #
oldeuro.FIN #
oldeuro.FRA #
oldeuro.GBR #
oldeuro.GRC #
oldeuro.IRL #
oldeuro.ITA #
oldeuro.LUX #
oldeuro.NLD #
oldeuro.PRT #
oldeuro.SWE #
oldeuro.LIE # Liechtenstein
oldeuro.NOR # Norway
oldeuro.CHE # Switzerland
oldeuro.GRL # Greenland (to Denmark)
oldeuro.ISL # Iceland
oldeuro.FRO # Island Feroe
oldeuro.ALA # Small islands/countries
oldeuro.AND #
oldeuro.GGY #
oldeuro.GIB #
oldeuro.IMN #
oldeuro.JEY #
oldeuro.MCO #
oldeuro.SJM #
oldeuro.SMR #
oldeuro.VAT #
oldeuro.SPM # St Pierre et Miquelon (to France)

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
te.KSV

usa.USA

* other small territories
kosau.CXR # Christmas Island (to Australia)
laca.ATA # Antarctica
ssa.ATF # French Southern Territories
laca.BMU # Bermuda
ssa.BVT # Bouvet Island
cajaz.COK # Cook Islands
kosau.HMD # Heard Island and McDonald Islands
ssa.IOT # British Indian Ocean Territory
kosau.NFK # Norfolk Island
cajaz.NIU # Niue
kosau.NRU # Nauru
cajaz.PCN # Pitcairn
laca.SGS # South Georgia and the South Sandwich Islands
laca.TCA # Turks and Caicos Islands
cajaz.TKL # Tokelau
cajaz.TUV # Tuvalu
cajaz.UMI # United States Minor Outlying Islands
laca.VGB # Virgin Islands, British
laca.VIR # Virgin Islands, U.S.
cajaz.WLF # Wallis and Futuna

/;


