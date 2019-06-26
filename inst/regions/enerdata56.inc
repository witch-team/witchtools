* Original Enerdata56 regions 

set enerdata56 'Regions' /
arg         # Argentina
aus         # Australia
aut
bel
bgr
blt			# Baltic States
bra         # Brazil
can         # Canada
chl         # Chile
chn         # China
cor         # South Korea
cro
dnk
egy         # Egypt  
esp
fin
fra
gbr
golf57      # Saudi Arabia, Iran, United Arab Emirates, Iraq, Kuwait, Oman, Qatar, Bahrain, Yemen
grc
hun
idn   		# Indonesia
irl
ita
jpn         # Japan
meme        # Israel, Jordan, Lebanon, Syria (NEED TO CUT AFTER BRAKETS)
mex         # Mexico
mys         # Malaysia
nde         # India
nld
noan        # Marocco and Tunisia
noap        # Algeria and Libya
nor
oeu			# Eastern Europe
osea        # Rest of South East Asia including Taiwan
pol
prt
rcam        # Rest of Central America
rcz
rfa
ris         # Rest of ex Former Union
rjan57      # New Zealand, Papua New Guinea and Pacific Islands
rom
rsaf        # all other countries in Adrica Sub Saharan Africa
rsam        # Rest of South America 
rsas        # Rest of South Asia 
rsl
rus         # Russia
slo
sui
swe
tha         # Thailand
tur    
ukr         # Ukraine
usa         # USA
vnm         # Vietnam
zaf         # South Africa    
/;

$batinclude 'regions/iso3.inc'

set map_enerdata56_iso3(enerdata56,iso3) 'Mapping between WITCH regions and countries/territories'/

rcam.BES
rcam.CUW
rcam.SXM

noan.ESH

aut.AUT # 
bel.BEL #
rfa.DEU #
dnk.DNK #
esp.ESP #
fin.FIN #
fra.FRA #
gbr.GBR #
grc.GRC #
irl.IRL #
ita.ITA #
nld.NLD #
prt.PRT #
swe.SWE #
bgr.BGR # 
rcz.CZE #
hun.HUN #
pol.POL #
rom.ROU #
rsl.SVK #
slo.SVN #
cro.HRV # Croatia

nor.NOR # Norway
sui.CHE # Switzerland
tur.TUR

blt.EST # Baltic states
blt.LTU #
blt.LVA #

oeu.CYP #
oeu.LUX #
oeu.MLT # Malta was formely in mena
oeu.LIE # Liechtenstein
oeu.GRL # Greenland (to Denmark)
oeu.ISL # Iceland
oeu.FRO # Island Feroe
oeu.ALA # Small islands/countries
oeu.AND #
oeu.GGY #
oeu.GIB #
oeu.IMN #
oeu.JEY #
oeu.MCO #
oeu.SJM #
oeu.SMR #
oeu.VAT #
oeu.SPM # St Pierre et Miquelon (to France)
oeu.BIH
oeu.ALB
oeu.MKD
oeu.MNE
oeu.SRB
oeu.KSV  # until now no ISO3 code has been assigned to Kosovo, but for future reference.

noap.LBY
noap.DZA
noan.TUN
noan.MAR
egy.EGY      	     
zaf.ZAF        

rsaf.AGO
rsaf.BEN
rsaf.BWA
rsaf.BFA
rsaf.BDI
rsaf.CMR
rsaf.CPV
rsaf.CAF
rsaf.TCD
rsaf.COM
rsaf.COG
rsaf.COD
rsaf.CIV
rsaf.GNQ
rsaf.ERI
rsaf.ETH
rsaf.GAB
rsaf.GMB
rsaf.GHA
rsaf.GIN
rsaf.GNB
rsaf.KEN
rsaf.LSO
rsaf.LBR
rsaf.MDG
rsaf.MWI
rsaf.MLI
rsaf.MRT
rsaf.MUS
rsaf.MYT
rsaf.MOZ
rsaf.NAM
rsaf.NER
rsaf.NGA
rsaf.REU
rsaf.RWA
rsaf.STP
rsaf.SEN
rsaf.SYC
rsaf.SHN
rsaf.SLE
rsaf.SOM
rsaf.SSD
rsaf.SDN
rsaf.SWZ
rsaf.TZA
rsaf.TGO
rsaf.UGA
rsaf.ZMB
rsaf.ZWE
rsaf.DJI
rsaf.IOT # British Indian Ocean Territory    # other small territories
rsaf.BVT # Bouvet Island
rsaf.ATF # French Southern Territories

meme.ISR
meme.JOR
meme.SYR
meme.LBN
meme.PSE # I added it

golf57.ARE
golf57.BHR
golf57.IRN
golf57.IRQ
golf57.KWT
golf57.OMN
golf57.QAT
golf57.SAU
golf57.YEM

aus.AUS # Australia

rjan57.CXR # Christmas Island (to Australia)           # Pacific Islands
rjan57.COK # Cook Islands
rjan57.HMD # Heard Island and McDonald Islands
rjan57.NFK # Norfolk Island
rjan57.NIU # Niue
rjan57.NRU # Nauru
rjan57.PCN # Pitcairn
rjan57.TKL # Tokelau
rjan57.TUV # Tuvalu
rjan57.UMI # United States Minor Outlying Islands
rjan57.WLF # Wallis and Futuna
rjan57.FJI
rjan57.PNG
rjan57.FSM
rjan57.GUM
rjan57.ASM 
rjan57.TLS
rjan57.PYF
rjan57.KIR
rjan57.MNP
rjan57.MHL
rjan57.NCL
rjan57.PLW
rjan57.WSM
rjan57.SLB
rjan57.TON
rjan57.VUT
rjan57.NZL # New Zealand

chn.CHN  # China
nde.IND # India
mys.MYS #Malaysia
tha.THA # thailand
vnm.VNM # Vietnam
jpn.JPN # Japan
cor.KOR # South Korea
idn.IDN  # Indonesia

osea.BRN   # East Asia
osea.CCK
osea.KHM
osea.LAO
osea.MMR
osea.PHL
osea.SGP
osea.PRK  # North-Korea
osea.HKG  # Small countries: Honk Kong
osea.MAC  #                  Macao
osea.TWN  #                  Taiwan
osea.MNG  # Mongolia

rsas.AFG # South Asia
rsas.BGD
rsas.BTN
rsas.LKA
rsas.MDV
rsas.NPL
rsas.PAK

arg.ARG
bra.BRA
chl.CHL

rsam.BOL
rsam.COL
rsam.ECU
rsam.FLK
rsam.GUF
rsam.GUY
rsam.PER
rsam.PRY
rsam.SUR
rsam.URY
rsam.VEN

mex.MEX

rcam.ABW
rcam.BHS
rcam.BLZ
rcam.BRB
rcam.CRI
rcam.CUB
rcam.DMA
rcam.DOM
rcam.GRD
rcam.GTM
rcam.HND
rcam.HTI
rcam.JAM
rcam.LCA
rcam.NIC
rcam.PAN
rcam.SLV
rcam.TTO
rcam.VCT
rcam.ATA # Antarctica
rcam.BMU # Bermuda
rcam.SGS # South Georgia and the South Sandwich Islands
rcam.TCA # Turks and Caicos Islands
rcam.VGB # Virgin Islands, British
rcam.VIR # Virgin Islands, U.S.
rcam.AIA
rcam.ATG
rcam.BLM
rcam.CYM
rcam.GLP
rcam.KNA
rcam.MAF
rcam.MSR
rcam.MTQ
rcam.PRI

can.CAN # Canada

usa.USA

rus.RUS
ukr.UKR

ris.ARM
ris.AZE
ris.BLR
ris.GEO
ris.KAZ
ris.KGZ
ris.MDA
ris.TJK
ris.TKM
ris.UZB

/;


