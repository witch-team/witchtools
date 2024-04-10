* EnerData 45 countries + 24(actually 26)(+2 added by me) regional aggregations (15 non-fundamental + 11(+2) fundamental)
* With all possible aggregations 73 regions, with only fundamental ones 58 regions

set ed58 'Regions' /
#Europe
#European Union
Austria
Belgium
Bulgaria
Croatia
Czechia
Denmark
Finland
France
Germany
Greece
Hungary
Ireland
Italy
Netherlands
Norway
Poland
Portugal
Romania
Slovakia
Slovenia
Spain
Sweden
Switzerland
Turkey
United Kingdom
#CIS
Russia
Ukraine
Rest of CIS
#North America
Canada
#Latin America
#South America
Argentina
Brazil
Chile
Rest of South America
#Central America and Caribbean
Mexico
Rest of Central America and Caribbean
#Asia
China
Japan
South Korea
India
Rest of South Asia
Indonesia
Malaysia
Thailand
Vietnam
Rest of South-East Asia
#Pacific
Australia
#Middle-East
Meditarrean
Rest of Gulf countries
Saudi Arabia
#Africa
#North Africa
Egypt
Algeria-Libya
Morocco-Tunisia
South Africa
Rest of Sub Saharan africa
#World
Baltics States
United States
#Non OECD
#OECD
Rest of Europe
Rest of Pacific
/;


$if set prepath      $batinclude '%prepath%regions/iso3.inc'
$if not set prepath  $batinclude 'regions/iso3.inc'

set map_ed58_iso3(ed58,iso3) 'Mapping between ed58 regions and countries/territories' /
#------------- EUROPE --------------

# --- European Union ---

    Austria.AUT #
    Belgium.BEL #
    Germany.DEU #
    Denmark.DNK #
    Spain.ESP #
    Finland.FIN #
    France.FRA #
    United Kingdom.GBR #
    Greece.GRC #
    Ireland.IRL #
    Italy.ITA #
    Netherlands.NLD #
    Portugal.PRT #
    Sweden.SWE #
    Bulgaria.BGR #
    Czechia.CZE #
    Hungary.HUN #
    Poland.POL #
    Romania.ROU #
    Slovakia.SVK #
    Slovenia.SVN #
    Croatia.HRV # Croatia was formely in TE

#--- other single european countries out of EU ---

    Norway.NOR # Norway
    Switzerland.CHE # Switzerland
    Turkey.TUR

#--- Baltic States ---

    Baltics States.EST # Baltic states
    Baltics States.LTU #
    Baltics States.LVA #


#---------- Rest of Europe ------------

# a) Cited by enerdata

#-- within EU --
    Rest of Europe.CYP # Cyprus
    Rest of Europe.LUX # Luxembourg
    Rest of Europe.MLT # Malta was formely in mena
#---------------
    Rest of Europe.ISL # Iceland

    Rest of Europe.KSV  # until now no ISO3 code has been assigned to Kosovo, but for future reference.
    Rest of Europe.SRB # Serbia
    Rest of Europe.ALB # Albania
    Rest of Europe.MNE # Montenegro
    Rest of Europe.MKD # Macedonia
    Rest of Europe.BIH # Bosnia-Herzegovina

# b) not cited by Enerdata --> are they included in Europe??

    Rest of Europe.LIE # Liechtenstein
    Rest of Europe.GRL # Greenland (to Denmark)
    Rest of Europe.FRO # Island Feroe
    Rest of Europe.ALA # Small islands/countries
    Rest of Europe.AND # Andorra
    Rest of Europe.GGY # Guernsey
    Rest of Europe.GIB # Gibraltar
    Rest of Europe.IMN # Isle of Man
    Rest of Europe.JEY # Jersey
    Rest of Europe.MCO # Monaco
    Rest of Europe.SJM # Svalbard and Jan Mayen
    Rest of Europe.SMR # San Marino
    Rest of Europe.VAT # Vatican
    Rest of Europe.SPM # St Pierre et Miquelon (to France)




#---------------- CIS ---------------

    Russia.RUS
    Ukraine.UKR

    Rest of CIS.ARM
    Rest of CIS.AZE
    Rest of CIS.BLR
    Rest of CIS.GEO
    Rest of CIS.KAZ
    Rest of CIS.KGZ
    Rest of CIS.MDA
    Rest of CIS.TJK
    Rest of CIS.TKM
    Rest of CIS.UZB




#---------------- AFRICA -----------------
    
# --- North Africa ---

    Egypt.EGY

    Algeria-Libya.LBY # Lybia
    Algeria-Libya.DZA # Algeria
    Morocco-Tunisia.TUN # Tunisia
    Morocco-Tunisia.MAR # Morocco

    Morocco-Tunisia.ESH # Western Sahara

    South Africa.ZAF # South Africa

    Rest of Sub Saharan africa.AGO
    Rest of Sub Saharan africa.BEN
    Rest of Sub Saharan africa.BWA
    Rest of Sub Saharan africa.BFA
    Rest of Sub Saharan africa.BDI
    Rest of Sub Saharan africa.CMR
    Rest of Sub Saharan africa.CPV
    Rest of Sub Saharan africa.CAF
    Rest of Sub Saharan africa.TCD
    Rest of Sub Saharan africa.COM
    Rest of Sub Saharan africa.COG
    Rest of Sub Saharan africa.COD
    Rest of Sub Saharan africa.CIV
    Rest of Sub Saharan africa.GNQ
    Rest of Sub Saharan africa.ERI
    Rest of Sub Saharan africa.ETH
    Rest of Sub Saharan africa.GAB
    Rest of Sub Saharan africa.GMB
    Rest of Sub Saharan africa.GHA
    Rest of Sub Saharan africa.GIN
    Rest of Sub Saharan africa.GNB
    Rest of Sub Saharan africa.KEN
    Rest of Sub Saharan africa.LSO
    Rest of Sub Saharan africa.LBR
    Rest of Sub Saharan africa.MDG
    Rest of Sub Saharan africa.MWI
    Rest of Sub Saharan africa.MLI
    Rest of Sub Saharan africa.MRT
    Rest of Sub Saharan africa.MUS
    Rest of Sub Saharan africa.MYT
    Rest of Sub Saharan africa.MOZ
    Rest of Sub Saharan africa.NAM
    Rest of Sub Saharan africa.NER
    Rest of Sub Saharan africa.NGA
    Rest of Sub Saharan africa.REU
    Rest of Sub Saharan africa.RWA
    Rest of Sub Saharan africa.STP
    Rest of Sub Saharan africa.SEN
    Rest of Sub Saharan africa.SYC
    Rest of Sub Saharan africa.SHN
    Rest of Sub Saharan africa.SLE
    Rest of Sub Saharan africa.SOM
    Rest of Sub Saharan africa.SSD
    Rest of Sub Saharan africa.SDN
    Rest of Sub Saharan africa.SWZ
    Rest of Sub Saharan africa.TZA
    Rest of Sub Saharan africa.TGO
    Rest of Sub Saharan africa.UGA
    Rest of Sub Saharan africa.ZMB
    Rest of Sub Saharan africa.ZWE
    Rest of Sub Saharan africa.DJI
    Rest of Sub Saharan africa.IOT # British Indian Ocean Territory    # other small territories
    Rest of Sub Saharan africa.BVT # Bouvet Island
    Rest of Sub Saharan africa.ATF # French Southern Territories




#--------------- MIDDLE EAST -----------------

    Mediterranean.ISR
    Mediterranean.JOR
    Mediterranean.SYR
    Mediterranean.LBN
    Mediterranean.PSE # I added it (Palestinian territory, occupied)

    Saudi Arabia.SAU

    Rest of Gulf countries.ARE # United Arab Emirates
    Rest of Gulf countries.BHR # Bahrain
    Rest of Gulf countries.IRN # Iran
    Rest of Gulf countries.IRQ # Iraq
    Rest of Gulf countries.KWT # Kuwait
    Rest of Gulf countries.OMN # Oman
    Rest of Gulf countries.QAT # Qatar
    Rest of Gulf countries.YEM # Yemen



#------------------ PACIFIC -------------------

    Australia.AUS # Australia

    Rest of Pacific.CXR # Christmas Island (to Australia)
    Rest of Pacific.COK # Cook Islands
    Rest of Pacific.HMD # Heard Island and McDonald Islands
    Rest of Pacific.NFK # Norfolk Island
    Rest of Pacific.NIU # Niue
    Rest of Pacific.NRU # Nauru
    Rest of Pacific.PCN # Pitcairn
    Rest of Pacific.TKL # Tokelau
    Rest of Pacific.TUV # Tuvalu
    Rest of Pacific.UMI # United States Minor Outlying Islands
    Rest of Pacific.WLF # Wallis and Futuna
    Rest of Pacific.FJI
    Rest of Pacific.PNG
    Rest of Pacific.FSM
    Rest of Pacific.GUM
    Rest of Pacific.ASM
    Rest of Pacific.TLS
    Rest of Pacific.PYF
    Rest of Pacific.KIR
    Rest of Pacific.MNP
    Rest of Pacific.MHL
    Rest of Pacific.NCL
    Rest of Pacific.PLW
    Rest of Pacific.WSM
    Rest of Pacific.SLB
    Rest of Pacific.TON
    Rest of Pacific.VUT
    Rest of Pacific.NZL # New Zealand



#------------------ ASIA --------------------

#--- South-East Asia ---

    Malaysia.MYS #Malaysia
    Thailand.THA # Thailand
    Vietnam.VNM # Vietnam
    Indonesia.IDN  # Indonesia

    Rest of South-East Asia.BRN  # Brunei
    Rest of South-East Asia.CCK  # Cocos (keeling) Islands
    Rest of South-East Asia.KHM  # Cambodia
    Rest of South-East Asia.LAO  # Laos
    Rest of South-East Asia.MMR  # Myanmar
    Rest of South-East Asia.PHL  # Philippines
    Rest of South-East Asia.SGP  # Singapore
    Rest of South-East Asia.PRK  # North-Korea
    Rest of South-East Asia.HKG  # Small countries: Honk Kong
    Rest of South-East Asia.MAC  #                  Macao
    Rest of South-East Asia.TWN  #                  Taiwan
    Rest of South-East Asia.MNG  # Mongolia

#--- South Asia ---

    India.IND # India

    Rest of South Asia.AFG # Afghanitan
    Rest of South Asia.BGD # Bangladesh
    Rest of South Asia.BTN # Bhutan
    Rest of South Asia.LKA # Sri-Lanka
    Rest of South Asia.MDV # Maldives
    Rest of South Asia.NPL # Nepal
    Rest of South Asia.PAK # Pakistan

#--- Other Asia ---

    China.CHN  # China
    Japan.JPN # Japan
    South Korea.KOR # South Korea




#---------------- LATIN AMERICA --------------

#---  South America ---

    Argentina.ARG
    Brazil.BRA
    Chile.CHL

    Rest of South America.BOL
    Rest of South America.COL
    Rest of South America.ECU
    Rest of South America.FLK
    Rest of South America.GUF
    Rest of South America.GUY
    Rest of South America.PER
    Rest of South America.PRY
    Rest of South America.SUR
    Rest of South America.URY
    Rest of South America.VEN

#--- Central America and Caribbean ---

    Mexico.MEX

    Rest of Central America and Caribbean.BES
    Rest of Central America and Caribbean.CUW
    Rest of Central America and Caribbean.SXM
    Rest of Central America and Caribbean.ABW
    Rest of Central America and Caribbean.BHS
    Rest of Central America and Caribbean.BLZ
    Rest of Central America and Caribbean.BRB
    Rest of Central America and Caribbean.CRI
    Rest of Central America and Caribbean.CUB
    Rest of Central America and Caribbean.DMA
    Rest of Central America and Caribbean.DOM
    Rest of Central America and Caribbean.GRD
    Rest of Central America and Caribbean.GTM
    Rest of Central America and Caribbean.HND
    Rest of Central America and Caribbean.HTI
    Rest of Central America and Caribbean.JAM
    Rest of Central America and Caribbean.LCA
    Rest of Central America and Caribbean.NIC
    Rest of Central America and Caribbean.PAN
    Rest of Central America and Caribbean.SLV
    Rest of Central America and Caribbean.TTO
    Rest of Central America and Caribbean.VCT
    Rest of Central America and Caribbean.ATA # Antarctica
    Rest of Central America and Caribbean.BMU # Bermuda
    Rest of Central America and Caribbean.SGS # South Georgia and the South Sandwich Islands
    Rest of Central America and Caribbean.TCA # Turks and Caicos Islands
    Rest of Central America and Caribbean.VGB # Virgin Islands, British
    Rest of Central America and Caribbean.VIR # Virgin Islands, U.S.
    Rest of Central America and Caribbean.AIA
    Rest of Central America and Caribbean.ATG
    Rest of Central America and Caribbean.BLM
    Rest of Central America and Caribbean.CYM
    Rest of Central America and Caribbean.GLP
    Rest of Central America and Caribbean.KNA
    Rest of Central America and Caribbean.MAF
    Rest of Central America and Caribbean.MSR
    Rest of Central America and Caribbean.MTQ
    Rest of Central America and Caribbean.PRI

#--- North America ---

    Canada.CAN # Canada
    United States.USA

    
/;
