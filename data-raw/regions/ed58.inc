* EnerData 45 countries + 24(+2) regional aggregations
* With all possible aggregations 73 regions, with only fundamental ones 58 regions

set ed58 'Regions' /
    arg         # Argentina
    aus         # Australia
    aut         # Austria
    bel         # Belgium
    bgr         # Bulgaria
    bra         # Brazil
    can         # Canada
    chl         # Chile
    chn         # China
    cor         # South Korea
    cro         # Croatia
    dnk         # Denmark
    egy         # Egypt
    esp         # Spain
    fin         # Finland
    fra         # France
    gbr         # UK
    golf57      # Iran, Arab Emirates, Iraq, Kuwait, Oman, Qatar, Bahrain, Yemen (GROUP)
    grc         # Greece
    hun         # Hungary
    idn         # Indonesia
    irl         # Ireland
    ita         # Italy
    jpn         # Japan
    meme        # Israel, Jordan, Lebanon, Syria (GROUP)
    mex         # Mexico
    mys         # Malaysia
    nde         # India
    nld         # The Netherlands
    noan        # Marocco and Tunisia
    noap        # Algeria and Libya
    nor         # Norway
    osea        # Rest of South East Asia including Taiwan (GROUP)
    pol         # Poland
    prt         # Portugal
    rcam        # Rest of Central America (GROUP)
    rcz         # Czech Republic
    rfa         # Germany
    ris         # Rest of ex Former Union (GROUP)
    rjan57      # New Zealand, Papua New Guinea and Pacific Islands (GROUP)
    rom         # Romania
    rsaf        # all other countries in Sub Saharan Africa  (GROUP)
    rsam        # Rest of South America (GROUP)
    rsas        # Rest of South Asia (GROUP)
    rsl         # Slovakia
    rus         # Russia
    slo         # Slovenia
    sui         # Switzerland
    swe         # Sweden
    tha         # Thailand
    tur         # Turkey
    ukr         # Ukraine
    usa         # USA
    vnm         # Vietnam
    sau         # Saudi Arabia
    zaf         # South Africa
    blt         # Baltic States (GROUP)
    oeu         # Eastern Europe (GROUP)
/;


$if set prepath      $batinclude '%prepath%regions/iso3.inc'
$if not set prepath  $batinclude 'regions/iso3.inc'



set map_ed58_iso3(ed58,iso3) 'Mapping between ed71 regions and countries/territories' /



#------------- EUROPE --------------

# --- European Union ---

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
    cro.HRV # Croatia was formely in TE

#--- other single european countries out of EU ---

    nor.NOR # Norway
    sui.CHE # Switzerland
    tur.TUR

#--- Baltic States ---

    blt.EST # Baltic states
    blt.LTU #
    blt.LVA #


#---------- oeu ------------

# a) Cited by enerdata

#-- within EU --
    oeu.CYP # Cyprus
    oeu.LUX # Luxembourg
    oeu.MLT # Malta was formely in mena
#---------------
    oeu.ISL # Iceland

    oeu.KSV  # until now no ISO3 code has been assigned to Kosovo, but for future reference.
    oeu.SRB # Serbia
    oeu.ALB # Albania
    oeu.MNE # Montenegro
    oeu.MKD # Macedonia
    oeu.BIH # Bosnia-Herzegovina

# b) not cited by Enerdata --> are they included in Europe??

    oeu.LIE # Liechtenstein
    oeu.GRL # Greenland (to Denmark)
    oeu.FRO # Island Feroe
    oeu.ALA # Small islands/countries
    oeu.AND # Andorra
    oeu.GGY # Guernsey
    oeu.GIB # Gibraltar
    oeu.IMN # Isle of Man
    oeu.JEY # Jersey
    oeu.MCO # Monaco
    oeu.SJM # Svalbard and Jan Mayen
    oeu.SMR # San Marino
    oeu.VAT # Vatican
    oeu.SPM # St Pierre et Miquelon (to France)




#---------------- CIS ---------------

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




#---------------- AFRICA -----------------
    
# --- North Africa ---

    egy.EGY

    noap.LBY # Lybia
    noap.DZA # Algeria
    noan.TUN # Tunisia
    noan.MAR # Morocco
    noan.ESH # Western Sahara

    zaf.ZAF # South Africa

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




#--------------- MIDDLE EAST -----------------

    meme.ISR
    meme.JOR
    meme.SYR
    meme.LBN
    meme.PSE # I added it (Palestinian territory, occupied)

    sau.SAU

    golf57.ARE # United Arab Emirates
    golf57.BHR # Bahrain
    golf57.IRN # Iran
    golf57.IRQ # Iraq
    golf57.KWT # Kuwait
    golf57.OMN # Oman
    golf57.QAT # Qatar
    golf57.YEM # Yemen



#------------------ PACIFIC -------------------

    aus.AUS # Australia

    rjan57.CXR # Christmas Island (to Australia)
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



#------------------ ASIA --------------------

#--- South-East Asia ---

    mys.MYS #Malaysia
    tha.THA # thailand
    vnm.VNM # Vietnam
    idn.IDN  # Indonesia

    osea.BRN  # Brunei
    osea.CCK  # Cocos (keeling) Islands
    osea.KHM  # Cambodia
    osea.LAO  # Laos
    osea.MMR  # Myanmar
    osea.PHL  # Philippines
    osea.SGP  # Singapore
    osea.PRK  # North-Korea
    osea.HKG  # Small countries: Honk Kong
    osea.MAC  #                  Macao
    osea.TWN  #                  Taiwan
    osea.MNG  # Mongolia

#--- South Asia ---

    India.IND # India

    rsas.AFG # Afghanitan
    rsas.BGD # Bangladesh
    rsas.BTN # Bhutan
    rsas.LKA # Sri-Lanka
    rsas.MDV # Maldives
    rsas.NPL # Nepal
    rsas.PAK # Pakistan

#--- Other Asia ---

    chn.CHN  # China
    jpn.JPN # Japan
    cor.KOR # South Korea




#---------------- LATIN AMERICA --------------

#---  South America ---

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

#--- Central America and Caribbean ---

    mex.MEX

    rcam.BES
    rcam.CUW
    rcam.SXM
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

#--- North America ---

    can.CAN # Canada
    usa.USA

/;
