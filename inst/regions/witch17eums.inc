* Original WITCH 17 regions new since April 2017

set witch17eums 'Regions' /
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
BGR
CYP
CZE
EST
HUN
LTU
LVA
POL
ROU
SVK
SVN
MLT
HRV
BEL
DEU
DNK
ESP
FIN
FRA
GBR
GRC
IRL
ITA
LUX
NLD
PRT
SWE
NOR
CHE
ISL
/;

$batinclude 'regions/iso3.inc'

set map_witch17eums_iso3(witch17eums,iso3) 'Mapping between WITCH regions and countries/territories'/

BGR.BGR # EU countries (except EU-15+Malta)
CYP.CYP #
CZE.CZE #
EST.EST #
HUN.HUN #
LTU.LTU #
LVA.LVA #
POL.POL #
ROU.ROU #
SVK.SVK #
SVN.SVN #
MLT.MLT # Malta was formely in mena
HRV.HRV # Croatia was formely in TE

AUT.AUT # EU-15 Countries
BEL.BEL #
DEU.DEU #
DNK.DNK #
ESP.ESP #
FIN.FIN #
FRA.FRA #
GBR.GBR #
GRC.GRC #
IRL.IRL #
ITA.ITA #
LUX.LUX #
NLD.NLD #
PRT.PRT #
SWE.SWE #
CHE.LIE # Liechtenstein
NOR.NOR # Norway
CHE.CHE # Switzerland
DNK.GRL # Greenland (to Denmark)
ISL.ISL # Iceland
GBR.FRO # Island Feroe
FIN.ALA # Small islands/countries
FRA.AND #
GBR.GGY #
ESP.GIB #
GBR.IMN #
GBR.JEY #
FRA.MCO #
SJM.SJM #
ITA.SMR #
ITA.VAT #

te.BIH
te.ALB
te.MKD
te.MNE
te.SRB
te.KSV 

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
te.TUR
te.TKM
te.UKR
te.UZB

usa.USA

/;





* ISO3 codes

set witch17eums 'Country definition in ISO_3166-1_alpha-3' /
ABW     'Aruba'
AFG     'Afghanistan'
AGO     'Angola'
AIA     'Anguilla'
ALA     'Åland Islands'
ALB     'Albania'
AND     'Andorra'
ARE     'United Arab Emirates'
ARG     'Argentina'
ARM     'Armenia'
ASM     'American Samoa'
ATA     'Antarctica'
ATF     'French Southern Territories'
ATG     'Antigua and Barbuda'
AUS     'Australia'
AUT     'Austria'
AZE     'Azerbaijan'
BDI     'Burundi'
BEL     'Belgium'
BEN     'Benin'
BES     'Bonaire, Sint Eustatius and Saba'
BFA     'Burkina Faso'
BGD     'Bangladesh'
BGR     'Bulgaria'
BHR     'Bahrain'
BHS     'Bahamas'
BIH     'Bosnia and Herzegovina'
BLM     'Saint Barthélemy'
BLR     'Belarus'
BLZ     'Belize'
BMU     'Bermuda'
BOL     'Bolivia, Plurinational State of'
BRA     'Brazil'
BRB     'Barbados'
BRN     'Brunei Darussalam'
BTN     'Bhutan'
BVT     'Bouvet Island'
BWA     'Botswana'
CAF     'Central African Republic'
CAN     'Canada'
CCK     'Cocos (Keeling) Islands'
CHE     'Switzerland'
CHL     'Chile'
CHN     'China'
CIV     'Côte d Ivoire'
CMR     'Cameroon'
COD     'Congo, the Democratic Republic of the'
COG     'Congo'
COK     'Cook Islands'
COL     'Colombia'
COM     'Comoros'
CPV     'Cape Verde'
CRI     'Costa Rica'
CUB     'Cuba'
CUW     'Curaçao'
CXR     'Christmas Island'
CYM     'Cayman Islands'
CYP     'Cyprus'
CZE     'Czech Republic'
DEU     'Germany'
DJI     'Djibouti'
DMA     'Dominica'
DNK     'Denmark'
DOM     'Dominican Republic'
DZA     'Algeria'
ECU     'Ecuador'
EGY     'Egypt'
ERI     'Eritrea'
ESH     'Western Sahara'
ESP     'Spain'
EST     'Estonia'
ETH     'Ethiopia'
FIN     'Finland'
FJI     'Fiji'
FLK     'Falkland Islands (Malvinas)'
FRA     'France'
FRO     'Faroe Islands'
FSM     'Micronesia, Federated States of'
GAB     'Gabon'
GBR     'United Kingdom'
GEO     'Georgia'
GGY     'Guernsey'
GHA     'Ghana  '
GIB     'Gibraltar'
GIN     'Guinea'
GLP     'Guadeloupe'
GMB     'Gambia'
GNB     'Guinea-Bissau'
GNQ     'Equatorial Guinea'
GRC     'Greece'
GRD     'Grenada'
GRL     'Greenland'
GTM     'Guatemala'
GUF     'French Guiana'
GUM     'Guam'
GUY     'Guyana'
HKG     'Hong Kong'
HMD     'Heard Island and McDonald Islands'
HND     'Honduras'
HRV     'Croatia'
HTI     'Haiti'
HUN     'Hungary'
IDN     'Indonesia'
IMN     'Isle of Man'
IND     'India'
IOT     'British Indian Ocean Territory'
IRL     'Ireland'
IRN     'Iran, Islamic Republic of'
IRQ     'Iraq'
ISL     'Iceland'
ISR     'Israel'
ITA     'Italy'
JAM     'Jamaica'
JEY     'Jersey'
JOR     'Jordan'
JPN     'Japan'
KAZ     'Kazakhstan'
KEN     'Kenya'
KGZ     'Kyrgyzstan'
KHM     'Cambodia'
KIR     'Kiribati'
KNA     'Saint Kitts and Nevis'
KOR     'Korea, Republic of'
KWT     'Kuwait'
LAO     'Lao People s Democratic Republic'
LBN     'Lebanon'
LBR     'Liberia'
LBY     'Libya'
LCA     'Saint Lucia'
LIE     'Liechtenstein'
LKA     'Sri Lanka'
LSO     'Lesotho'
LTU     'Lithuania'
LUX     'Luxembourg'
LVA     'Latvia'
MAC     'Macao'
MAF     'Saint Martin (French part)'
MAR     'Morocco'
MCO     'Monaco'
MDA     'Moldova, Republic of'
MDG     'Madagascar'
MDV     'Maldives'
MEX     'Mexico'
MHL     'Marshall Islands'
MKD     'Macedonia, the former Yugoslav Republic of'
MLI     'Mali'
MLT     'Malta'
MMR     'Myanmar'
MNE     'Montenegro'
MNG     'Mongolia'
MNP     'Northern Mariana Islands'
MOZ     'Mozambique'
MRT     'Mauritania'
MSR     'Montserrat'
MTQ     'Martinique'
MUS     'Mauritius'
MWI     'Malawi'
MYS     'Malaysia'
MYT     'Mayotte'
NAM     'Namibia'
NCL     'New Caledonia'
NER     'Niger'
NFK     'Norfolk Island'
NGA     'Nigeria'
NIC     'Nicaragua'
NIU     'Niue'
NLD     'Netherlands'
NOR     'Norway'
NPL     'Nepal'
NRU     'Nauru'
NZL     'New Zealand'
OMN     'Oman'
PAK     'Pakistan'
PAN     'Panama'
PCN     'Pitcairn'
PER     'Peru'
PHL     'Philippines'
PLW     'Palau'
PNG     'Papua New Guinea'
POL     'Poland'
PRI     'Puerto Rico'
PRK     'Korea, Democratic Peoples Republic of'
PRT     'Portugal'
PRY     'Paraguay'
PSE     'Palestine, State of'
PYF     'French Polynesia'
QAT     'Qatar'
REU     'Réunion'
ROU     'Romania'
RUS     'Russian Federation'
RWA     'Rwanda'
SAU     'Saudi Arabia'
SDN     'Sudan'
SEN     'Senegal'
SGP     'Singapore'
SGS     'South Georgia and the South Sandwich Islands'
SHN     'Saint Helena, Ascension and Tristan da Cunha'
SJM     'Svalbard and Jan Mayen'
SLB     'Solomon Islands'
SLE     'Sierra Leone'
SLV     'El Salvador'
SMR     'San Marino'
SOM     'Somalia'
SPM     'Saint Pierre and Miquelon'
SRB     'Serbia'
SSD     'South Sudan'
STP     'Sao Tome and Principe'
SUR     'Suriname'
SVK     'Slovakia'
SVN     'Slovenia'
SWE     'Sweden'
SWZ     'Swaziland'
SXM     'Sint Maarten (Dutch part)'
SYC     'Seychelles'
SYR     'Syrian Arab Republic'
TCA     'Turks and Caicos Islands'
TCD     'Chad'
TGO     'Togo'
THA     'Thailand'
TJK     'Tajikistan'
TKL     'Tokelau'
TKM     'Turkmenistan'
TLS     'Timor-Leste'
TON     'Tonga'
TTO     'Trinidad and Tobago'
TUN     'Tunisia'
TUR     'Turkey'
TUV     'Tuvalu'
TWN     'Taiwan, Province of China'
TZA     'Tanzania, United Republic of'
UGA     'Uganda'
UKR     'Ukraine'
UMI     'United States Minor Outlying Islands'
URY     'Uruguay'
USA     'United States'
UZB     'Uzbekistan'
VAT     'Holy See (Vatican City State)'
VCT     'Saint Vincent and the Grenadines'
VEN     'Venezuela, Bolivarian Republic of'
VGB     'Virgin Islands, British'
VIR     'Virgin Islands, U.S.'
VNM     'Viet Nam'
VUT     'Vanuatu'
WLF     'Wallis and Futuna'
WSM     'Samoa'
YEM     'Yemen'
ZAF     'South Africa'
ZMB     'Zambia'
ZWE     'Zimbabwe'
# additional countries
KSV     'Kosovo' 
WBG		'Palestinian West Bank and Gaza'
/;

$batinclude 'regions/iso3.inc'

set map_cdlinksg20_iso3(cdlinksg20,iso3) 'Mapping between LIMITS10 regions and countries/territories'/
ABW.ABW
AFG.AFG
AGO.AGO
AIA.AIA
ALA.ALA
ALB.ALB
AND.AND
ARE.ARE
ARG.ARG
ARM.ARM
ASM.ASM
ATA.ATA
ATF.ATF
ATG.ATG
AUS.AUS
AUT.AUT
AZE.AZE
BDI.BDI
BEL.BEL
BEN.BEN
BES.BES
BFA.BFA
BGD.BGD
BGR.BGR
BHR.BHR
BHS.BHS
BIH.BIH
BLM.BLM
BLR.BLR
BLZ.BLZ
BMU.BMU
BOL.BOL
BRA.BRA
BRB.BRB
BRN.BRN
BTN.BTN
BVT.BVT
BWA.BWA
CAF.CAF
CAN.CAN
CCK.CCK
CHE.CHE
CHL.CHL
CHN.CHN
CIV.CIV
CMR.CMR
COD.COD
COG.COG
COK.COK
COL.COL
COM.COM
CPV.CPV
CRI.CRI
CUB.CUB
CUW.CUW
CXR.CXR
CYM.CYM
CYP.CYP
CZE.CZE
DEU.DEU
DJI.DJI
DMA.DMA
DNK.DNK
DOM.DOM
DZA.DZA
ECU.ECU
EGY.EGY
ERI.ERI
ESH.ESH
ESP.ESP
EST.EST
ETH.ETH
FIN.FIN
FJI.FJI
FLK.FLK
FRA.FRA
FRO.FRO
FSM.FSM
GAB.GAB
GBR.GBR
GEO.GEO
GGY.GGY
GHA.GHA
GIB.GIB
GIN.GIN
GLP.GLP
GMB.GMB
GNB.GNB
GNQ.GNQ
GRC.GRC
GRD.GRD
GRL.GRL
GTM.GTM
GUF.GUF
GUM.GUM
GUY.GUY
HKG.HKG
HMD.HMD
HND.HND
HRV.HRV
HTI.HTI
HUN.HUN
IDN.IDN
IMN.IMN
IND.IND
IOT.IOT
IRL.IRL
IRN.IRN
IRQ.IRQ
ISL.ISL
ISR.ISR
ITA.ITA
JAM.JAM
JEY.JEY
JOR.JOR
JPN.JPN
KAZ.KAZ
KEN.KEN
KGZ.KGZ
KHM.KHM
KIR.KIR
KNA.KNA
KOR.KOR
KWT.KWT
LAO.LAO
LBN.LBN
LBR.LBR
LBY.LBY
LCA.LCA
LIE.LIE
LKA.LKA
LSO.LSO
LTU.LTU
LUX.LUX
LVA.LVA
MAC.MAC
MAF.MAF
MAR.MAR
MCO.MCO
MDA.MDA
MDG.MDG
MDV.MDV
MEX.MEX
MHL.MHL
MKD.MKD
MLI.MLI
MLT.MLT
MMR.MMR
MNE.MNE
MNG.MNG
MNP.MNP
MOZ.MOZ
MRT.MRT
MSR.MSR
MTQ.MTQ
MUS.MUS
MWI.MWI
MYS.MYS
MYT.MYT
NAM.NAM
NCL.NCL
NER.NER
NFK.NFK
NGA.NGA
NIC.NIC
NIU.NIU
NLD.NLD
NOR.NOR
NPL.NPL
NRU.NRU
NZL.NZL
OMN.OMN
PAK.PAK
PAN.PAN
PCN.PCN
PER.PER
PHL.PHL
PLW.PLW
PNG.PNG
POL.POL
PRI.PRI
PRK.PRK
PRT.PRT
PRY.PRY
PSE.PSE
PYF.PYF
QAT.QAT
REU.REU
ROU.ROU
RUS.RUS
RWA.RWA
SAU.SAU
SDN.SDN
SEN.SEN
SGP.SGP
SGS.SGS
SHN.SHN
SJM.SJM
SLB.SLB
SLE.SLE
SLV.SLV
SMR.SMR
SOM.SOM
SPM.SPM
SRB.SRB
SSD.SSD
STP.STP
SUR.SUR
SVK.SVK
SVN.SVN
SWE.SWE
SWZ.SWZ
SXM.SXM
SYC.SYC
SYR.SYR
TCA.TCA
TCD.TCD
TGO.TGO
THA.THA
TJK.TJK
TKL.TKL
TKM.TKM
TLS.TLS
TON.TON
TTO.TTO
TUN.TUN
TUR.TUR
TUV.TUV
TWN.TWN
TZA.TZA
UGA.UGA
UKR.UKR
UMI.UMI
URY.URY
USA.USA
UZB.UZB
VAT.VAT
VCT.VCT
VEN.VEN
VGB.VGB
VIR.VIR
VNM.VNM
VUT.VUT
WLF.WLF
WSM.WSM
YEM.YEM
ZAF.ZAF
ZMB.ZMB
ZWE.ZWE
KSV.KSV
WBG.WBG

/;


