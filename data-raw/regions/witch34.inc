* WITCH17 plus 14 European single countries (all countries with more than 10 million inhabitants) and 4 European regions

set witch34 'Regions' /
usa           # United States of America
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
northeu       # Northern Europe (ETS)
easteu        # Eastern Europe (ETS)
bnl           # BeNeLux (ETS)
balkan        # Balkan countries (non-ETS)
AUT
CHE           # ETS
DEU
GBR           # non-ETS
FRA
IRL
ITA
ESP
POL
ROU
CZE
GRC
PRT
SWE
/;

$batinclude 'regions/iso3.inc'

set map_witch34_iso3(witch34,iso3) 'Mapping between WITCH regions and countries/territories'/

AUT.AUT #
CHE.CHE # Switzerland
CZE.CZE #
POL.POL #
ROU.ROU #
DEU.DEU #
ESP.ESP #
FRA.FRA #
GBR.GBR #
GRC.GRC #
IRL.IRL #
ITA.ITA #
PRT.PRT #
SWE.SWE #

FRA.AND # small territories/countries
GBR.GGY #
ESP.GIB #
GBR.IMN #
GBR.JEY #
FRA.MCO #
ITA.SMR #
ITA.VAT #
CHE.LIE # Liechtenstein

bnl.NLD #
bnl.BEL #
bnl.LUX #

easteu.BGR # Eastern European EU countries
easteu.CYP #
easteu.EST #
easteu.HUN #
easteu.LTU #
easteu.LVA #
easteu.SVK #
easteu.SVN #
easteu.MLT # Malta was formely in mena
easteu.HRV # Croatia was formely in TE

northeu.DNK #
northeu.FIN #
northeu.ISL # Iceland
northeu.NOR # Norway
northeu.GRL # Greenland (to Denmark)
northeu.FRO # Island Feroe (to Denmark)
northeu.ALA # (to Finland)
northeu.SJM # (to Norway)

balkan.BIH
balkan.ALB
balkan.MKD
balkan.MNE
balkan.SRB
balkan.KSV

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
te.TKM
te.TUR
te.UKR
te.UZB

usa.USA

/;
