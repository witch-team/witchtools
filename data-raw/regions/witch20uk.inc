* WITCH20UK
* Region aggregation based on WITCH20, but including UK in the European Union -> EU28

set witch20uk 'Regions' /
brazil        # Brazil
canada        # Canada
ccasia        # Caucasus and Central Asia
china         # China
eu28          # European Union
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

set map_witch20uk_iso3(witch20uk,iso3) 'Mapping between WITCH regions and countries/territories'/

eu28.AUT #
eu28.CZE #
eu28.POL #
eu28.ROU #
eu28.DEU #
eu28.ESP #
eu28.FRA #
eu28.GRC #
eu28.IRL #
eu28.ITA #
eu28.PRT #
eu28.SWE #
eu28.NLD #
eu28.BEL #
eu28.LUX #
eu28.BGR # Eastern European EU countries
eu28.CYP #
eu28.EST #
eu28.HUN #
eu28.LTU #
eu28.LVA #
eu28.SVK #
eu28.SVN #
eu28.MLT # Malta was formely in mena
eu28.HRV # Croatia was formely in TE
eu28.DNK #
eu28.FIN #

eu28.AND # small territories/countries
eu28.MCO #
eu28.SMR #
eu28.VAT #
eu28.GRL # Greenland (to Denmark)
eu28.FRO # Island Feroe (to Denmark)
eu28.ALA # (to Finland)

othereurope.CHE # Switzerland
othereurope.LIE # Liechtenstein
eu28.GBR #
eu28.GGY #
eu28.GIB #
eu28.IMN #
eu28.JEY #
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
