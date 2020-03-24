* IMAGE 26 regions

set image26 'Regions' /
bra   # Brazil
can   # Canada
ceu   # Central Europe
chn   # China (Mainland, Hongkong, Macao; excl. Taiwan)
eaf   # Eastern Africa
india # India
indo  # Indonesia
jap   # Japan
kor   # South Korea
me    # Middle East
mex   # Mexico
naf   # Northern Africa
oce   # Oceania (including Australia and New Zealand)
rcam  # Central America
rsaf  # Rest of Southern Africa (excl. South Africa)
rsam  # Latin America (Excl. Mexico, Brazil; incl. Carribean islands)
rsas  # Rest of South Asia (Pakistan+Afganistan)
rus   # Russia
saf   # South Africa
seas  # Southeast Asia
stan  # Central Asia (-stan countries)
tur   # Turkey
ukr   # Ukraine region
usa   # United States of America
waf   # Western Africa
weu   # Western Europe
/;

$batinclude 'regions/iso3.inc'

set map_image26_iso3(image26,iso3) 'Mapping between IMAGE regions and countries/territories'/

bra.BRA
can.CAN # Canada
can.SPM # St Pierre et Miquelon (to France)
ceu.ALB
ceu.BGR # EU countries (except EU-15+Malta)
ceu.BIH
ceu.CYP #
ceu.CZE #
ceu.EST #
ceu.KSV
ceu.HRV # Croatia was formely in TE
ceu.HUN #
ceu.LTU #
ceu.LVA #
ceu.MKD
ceu.MLT # Malta was formely in mena
ceu.MNE
ceu.POL #
ceu.ROU #
ceu.SRB
ceu.SVK #
ceu.SVN #
chn.CHN  # China
chn.HKG  # Small countries: Honk Kong
chn.MAC  #                  Macao
chn.MNG  # Mongolia
chn.TWN  #                  Taiwan
eaf.BDI
eaf.DJI
eaf.ERI
eaf.ETH
eaf.IOT # British Indian Ocean Territory
eaf.KEN
eaf.MDG
eaf.MYS
eaf.REU
eaf.RWA
eaf.SDN
eaf.SOM
eaf.SSD
eaf.SYC
eaf.TZA
eaf.UGA
india.IND # India
indo.IDN # Indonesia
indo.PNG
indo.TLS
jap.JPN # Japan
kor.KOR # South Korea
kor.PRK
me.ARE
me.BHR
me.IRN
me.IRQ
me.ISR
me.JOR
me.KWT
me.LBN
me.OMN
me.PSE
me.QAT
me.SAU
me.SYR
me.YEM
mex.MEX
naf.BMU # Bermuda
naf.DZA
naf.EGY
naf.ESH
naf.LBY
naf.MAR
naf.TUN
oce.ASM
oce.AUS # Australia
oce.CCK
oce.COK # Cook Islands
oce.CXR # Christmas Island (to Australia)
oce.FJI
oce.FSM
oce.GUM
oce.HMD # Heard Island and McDonald Islands
oce.KIR
oce.MHL
oce.MNP
oce.NCL
oce.NFK # Norfolk Island
oce.NIU # Niue
oce.NRU # Nauru
oce.NZL # New Zealand
oce.PCN # Pitcairn
oce.PYF
oce.SLB
oce.TCA # Turks and Caicos Islands
oce.TKL # Tokelau
oce.TON
oce.TUV # Tuvalu
oce.UMI # United States Minor Outlying Islands
oce.VUT
oce.WLF # Wallis and Futuna
oce.WSM
rcam.ABW
rcam.AIA
rcam.ATG
rcam.BES
rcam.BHS
rcam.BLM
rcam.BLZ
rcam.BRB
rcam.CRI
rcam.CUB
rcam.CUW
rcam.CYM
rcam.DMA
rcam.DOM
rcam.FLK
rcam.GLP
rcam.GRD
rcam.GTM
rcam.HND
rcam.HTI
rcam.JAM
rcam.KNA
rcam.LCA
rcam.MAF
rcam.MSR
rcam.MTQ
rcam.NIC
rcam.PAN
rcam.PRI
rcam.SGS # South Georgia and the South Sandwich Islands
rcam.SLV
rcam.SXM
rcam.TTO
rcam.VCT
rcam.VGB # Virgin Islands, British
rcam.VIR # Virgin Islands, U.S.
rsaf.AGO
rsaf.BWA
rsaf.COM
rsaf.LSO
rsaf.MOZ
rsaf.MWI
rsaf.MYT
rsaf.NAM
rsaf.SWZ
rsaf.ZMB
rsaf.ZWE
rsam.ARG
rsam.ATA
rsam.BOL
rsam.CHL
rsam.COL
rsam.ECU
rsam.GUF
rsam.GUY
rsam.PER
rsam.PRY
rsam.SUR
rsam.URY
rsam.VEN
rsas.AFG
rsas.NPL
rsas.PAK
rus.ARM
rus.AZE
rus.GEO
rus.RUS
saf.ATF # French Southern Territories
saf.BVT # Bouvet Island
saf.ZAF # South Africa
seas.BGD
seas.BRN
seas.BTN
seas.KHM
seas.LAO
seas.LKA
seas.MDV
seas.MMR
seas.PHL
seas.PLW
seas.SGP
seas.THA
seas.VNM
stan.KAZ
stan.KGZ
stan.TJK
stan.TKM
stan.UZB
tur.TUR
ukr.BLR
ukr.MDA
ukr.UKR
usa.USA
waf.BEN
waf.BFA
waf.CAF
waf.CIV
waf.CMR
waf.COD
waf.COG
waf.CPV
waf.GAB
waf.GHA
waf.GIN
waf.GMB
waf.GNB
waf.GNQ
waf.LBR
waf.MLI
waf.MRT
waf.MUS
waf.NER
waf.NGA
waf.SEN
waf.SHN
waf.SLE
waf.STP
waf.TCD
waf.TGO
weu.ALA # Small islands countries
weu.AND #
weu.AUT # EU-15 Countries
weu.BEL #
weu.CHE # Switzerland
weu.DEU #
weu.DNK #
weu.ESP #
weu.FIN #
weu.FRA #
weu.FRO # Island Feroe
weu.GBR #
weu.GGY #
weu.GIB #
weu.GRC #
weu.GRL # Greenland (to Denmark)
weu.IMN #
weu.IRL #
weu.ISL # Iceland
weu.ITA #
weu.JEY #
weu.LIE # Liechtenstein
weu.LUX #
weu.MCO #
weu.NLD #
weu.NOR # Norway
weu.PRT #
weu.SJM #
weu.SMR #
weu.SWE #
weu.VAT #

/;


