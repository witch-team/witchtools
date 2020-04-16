* Original WITCH 17 regions new since April 2017

set witch17 'Regions' /
usa           # United States of America
europe        # Europe (EU+EFTA+Balkan countries and Turkey)
te            # Non-EU Eastern European and transition countries
mena          # Middle East and North Africa
ssa           # Sub Saharan Africa
china         # China, including Taiwan
seasia        # South East Asia (excluding Indonesia)
sasia         # South Asia excluding India
jpnkor        # Japan and Korea
laca          # Latin America and the Caribbean (excluding Mexico and Brazil)
india         # India
indonesia     # Indonesia
mexico        # Mexico
brazil        # Brazil
canada        # Canada
oceania       # Australia, New Zealand, and oceania islands
southafrica   # South Africa
/;

$batinclude 'regions/iso3.inc'

set map_witch17_iso3(witch17,iso3) 'Mapping between WITCH regions and countries/territories'/

canada.CAN # Canada
canada.SPM # St Pierre et Miquelon

jpnkor.JPN # Japan
jpnkor.KOR # South Korea

oceania.NZL # New Zealand
oceania.AUS # Australia

indonesia.IDN  # Indonesia

southafrica.ZAF # South Africa

brazil.BRA

mexico.MEX

china.CHN  # China
china.HKG  # Small countries: Honk Kong
china.MAC  #                  Macao
china.TWN  #                  Taiwan

india.IND # India

te.MNG   # Mongolia

ssa.DJI

ssa.IOT # British Indian Ocean Territory    # other small territories
ssa.BVT # Bouvet Island
ssa.ATF # French Southern Territories

laca.ATA # Antarctica
laca.BMU # Bermuda
laca.SGS # South Georgia and the South Sandwich Islands
laca.TCA # Turks and Caicos Islands
laca.VGB # Virgin Islands, British
laca.VIR # Virgin Islands, U.S.

jpnkor.PRK  # North-Korea

oceania.CXR # Christmas Island (to Australia)           # oceania Islands
oceania.COK # Cook Islands
oceania.HMD # Heard Island and McDonald Islands
oceania.NFK # Norfolk Island
oceania.NIU # Niue
oceania.NRU # Nauru
oceania.PCN # Pitcairn
oceania.TKL # Tokelau
oceania.TUV # Tuvalu
oceania.UMI # United States Minor Outlying Islands
oceania.WLF # Wallis and Futuna
oceania.FJI
oceania.PNG
oceania.FSM
oceania.GUM
oceania.ASM 
oceania.TLS
oceania.PYF
oceania.KIR
oceania.MNP
oceania.MHL
oceania.NCL
oceania.PLW
oceania.WSM
oceania.SLB
oceania.TON
oceania.VUT

sasia.AFG # South Asia
sasia.BGD
sasia.BTN
sasia.LKA
sasia.MDV
sasia.NPL
sasia.PAK

seasia.BRN   # East Asia
seasia.CCK
seasia.KHM
seasia.LAO
seasia.MMR
seasia.MYS
seasia.PHL
seasia.SGP
seasia.THA
seasia.VNM




laca.ABW
laca.AIA
laca.ARG
laca.ATG
laca.BES
laca.BHS
laca.BLM
laca.BLZ
laca.BOL
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

mena.ARE
mena.BHR
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

europe.BIH
europe.ALB
europe.MKD
europe.MNE
europe.SRB
europe.KSV 

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

te.ARM
te.AZE
te.BLR
te.GEO
te.KAZ
te.KGZ
te.MDA
te.RUS
te.TJK
te.TUR
te.TKM
te.UKR
te.UZB

usa.USA

/;


