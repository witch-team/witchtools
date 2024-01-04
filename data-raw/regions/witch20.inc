* WITCH20 regions

set witch20 'Regions' /
brazil        # Brazil
canada        # Canada
ccasia        # Caucasus and Central Asia
china         # China
eu27          # European Union
india         # India
indonesia     # Indonesia
japan         # Japan
korea         # Korea
laca          # Latin America and the Caribbean
mena          # Middle East and North Africa
mexico        # Mexico
oceania       # Australia, New Zealand, and Oceania islands
othereurope   # Other European countries
sasia         # South Asia
seasia        # South East Asia
southafrica   # South Africa
ssa           # Sub Saharan Africa
te            # Non-EU Eastern European and Transition countries
usa           # United States of America
/;

$batinclude 'regions/iso3.inc'

set map_witch20_iso3(witch20,iso3) 'Mapping between WITCH regions and countries/territories'/

eu27.AUT #
eu27.CZE #
eu27.POL #
eu27.ROU #
eu27.DEU #
eu27.ESP #
eu27.FRA #
eu27.GRC #
eu27.IRL #
eu27.ITA #
eu27.PRT #
eu27.SWE #
eu27.NLD #
eu27.BEL #
eu27.LUX #
eu27.BGR # Eastern European EU countries
eu27.CYP #
eu27.EST #
eu27.HUN #
eu27.LTU #
eu27.LVA #
eu27.SVK #
eu27.SVN #
eu27.MLT # Malta was formely in mena
eu27.HRV # Croatia was formely in TE
eu27.DNK #
eu27.FIN #

eu27.AND # small territories/countries
eu27.MCO #
eu27.SMR #
eu27.VAT #
eu27.GRL # Greenland (to Denmark)
eu27.FRO # Island Feroe (to Denmark)
eu27.ALA # (to Finland)

othereurope.CHE # Switzerland
othereurope.LIE # Liechtenstein
othereurope.GBR #
othereurope.GGY #
othereurope.GIB #
othereurope.IMN #
othereurope.JEY #
othereurope.ISL # Iceland
othereurope.NOR # Norway
othereurope.SJM # (to Norway)
othereurope.BIH
othereurope.ALB
othereurope.MKD
othereurope.MNE
othereurope.SRB
othereurope.KSV

canada.CAN # Canada
canada.SPM # St Pierre et Miquelon

japan.JPN # Japan
korea.KOR # South Korea

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

ccasia.MNG   # Mongolia

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

ccasia.PRK  # North-Korea

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

ccasia.ARM
ccasia.AZE
ccasia.GEO
ccasia.KAZ
ccasia.KGZ
ccasia.TJK
ccasia.TKM
ccasia.UZB

te.BLR
te.MDA
te.RUS
te.TUR
te.UKR

usa.USA

/;
