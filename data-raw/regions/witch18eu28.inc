* WITCH18EU28
* Region aggregation based on WITCH17, but separating the European Union + UK -> EU28

set witch18eu28 'Regions' /
brazil        # Brazil
canada        # Canada
china         # China
eu28          # EU27 + UK
othereurope   # European countries outside the EU28 (EFTA+Balkan countries and Turkey)
india         # India
indonesia     # Indonesia
jpnkor        # Japan and Korea
laca          # Latin America and the Caribbean
mena          # Middle East and North Africa
mexico        # Mexico
oceania       # Australia, New Zealand, and Oceania islands
sasia         # South Asia
seasia        # South East Asia
southafrica   # South Africa
ssa           # Sub Saharan Africa
te            # Non-EU Eastern European and Transition countries
usa           # United States of America
/;

$batinclude 'regions/iso3.inc'

set map_witch18eu28_iso3(witch18eu28,iso3) 'Mapping between WITCH regions and countries/territories'/

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


eu28.AUT # Austria
eu28.BEL # Belgium
eu28.BGR # Bulgaria
eu28.CYP # Republic of Cyprus
eu28.CZE # Czech Republic
eu28.DEU # Germany
eu28.DNK # Denmark
eu28.ESP # Spain
eu28.EST # Estonia
eu28.FIN # Finland
eu28.FRA # France
eu28.GRC # Greece
eu28.HRV # Croatia was formely in TE
eu28.HUN # Hungary
eu28.IRL # Ireland
eu28.ITA # Italy
eu28.LTU # Lithuania
eu28.LUX # Luxembourg
eu28.LVA # Latvia
eu28.MLT # Malta was formely in mena
eu28.NLD # Netherlands
eu28.POL # Poland
eu28.PRT # Portugal
eu28.ROU # Romania
eu28.SVK # Slovakia
eu28.SVN # Slovenia
eu28.SWE # Sweden

eu28.GBR # Great Britain

othereurope.LIE # Liechtenstein
othereurope.NOR # Norway
othereurope.CHE # Switzerland
eu28.GRL # Greenland (Denmark)
othereurope.ISL # Iceland
eu28.FRO # Island Feroe (Denmark)

 # Small islands/countries 
eu28.ALA # Åland Islands (Finland)
eu28.AND # Andorra 
eu28.GGY # Guernsey (UK)
eu28.GIB # Gibraltar (UK)
eu28.IMN # Island of Man (UK)
eu28.JEY # Jersey (UK)
eu28.MCO # Monaco
othereurope.SJM # Svalbard and Jan Mayen (Norway)
eu28.SMR # San Marino
eu28.VAT # Vatican City

othereurope.BIH
othereurope.ALB
othereurope.MKD
othereurope.MNE
othereurope.SRB
othereurope.KSV 

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


