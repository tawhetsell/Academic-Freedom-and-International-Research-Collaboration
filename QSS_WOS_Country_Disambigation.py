import os
import logging
from typing import Optional

import pandas as pd

# configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)

disambiguation_dict = {
    'abakaliki': 'NGA',  # City in Nigeria
    'abuja': 'NGA',  # Capital city of Nigeria
    'ackorea': 'KOR',  # Assuming typo for South Korea
    'acoruna': 'ESP',  # City in Spain
    'afghanistan': 'AFG',
    'alabama': 'USA',  # State in the USA
    'albania': 'ALB',
    'algeria': 'DZA',
    'amagasaki': 'JPN',  # City in Japan
    'ambia': None,  # Ambiguous, possibly a typo
    'andorra': 'AND',
    'angola': 'AGO',
    'anguilla': 'AIA',
    'ankara': 'TUR',  # Capital city of Turkey
    'antiguabarbu': 'ATG',  # Short for Antigua and Barbuda
    'argentina': 'ARG',
    'argentine': 'ARG',  # Another name for Argentina
    'armenia': 'ARM',
    'aruba': 'ABW',
    'ascensionislan': 'SHN',  # Ascension Island, part of Saint Helena
    'assam': 'IND',  # State in India
    'astar': None,  # Ambiguous
    'au': 'AUS',  # Assuming short for Australia
    'ausralia': 'AUS',  # Typo for Australia
    'austalia': 'AUS',  # Typo for Australia
    'austraia': 'AUS',  # Typo for Australia
    'australia': 'AUS',
    'australlia': 'AUS',  # Typo for Australia
    'austria': 'AUT',
    'austrialia': 'AUS',  # Typo for Australia
    'azerbaijan': 'AZE',
    'bahamas': 'BHS',
    'bahrain': 'BHR',
    'bangladeh': 'BGD',  # Typo for Bangladesh
    'bangladesh': 'BGD',
    'bangor': 'GBR',  # City in the UK
    'barbados': 'BRB',
    'bath': 'GBR',  # City in the UK
    'beckenham': 'GBR',  # Area in the UK
    'belarus': 'BLR',
    'belgium': 'BEL',
    'belize': 'BLZ',
    'benin': 'BEN',
    'berlin': 'DEU',  # Capital city of Germany
    'bermuda': 'BMU',
    'bhutan': 'BTN',
    'blyth': 'GBR',  # Town in the UK
    'boli': None,  # Ambiguous
    'bolivia': 'BOL',
    'boliviaplurin': 'BOL',  # Short for Bolivia Plurinational State
    'bonaire': 'BES',  # Bonaire, Sint Eustatius and Saba
    'bosnahercego': 'BIH',  # Short for Bosnia and Herzegovina
    'bosniaandherz': 'BIH',  # Short for Bosnia and Herzegovina
    'bosniaherceg': 'BIH',  # Short for Bosnia and Herzegovina
    'bosniaherzeg': 'BIH',  # Short for Bosnia and Herzegovina
    'bosniaherzegov': 'BIH',  # Short for Bosnia and Herzegovina
    'botswana': 'BWA',
    'brazil': 'BRA',
    'britishindian': 'IOT',  # British Indian Ocean Territory
    'britishisles': None,  # Geographic term, not a country
    'britishvirginisl': 'VGB',  # British Virgin Islands
    'brno': 'CZE',  # City in the Czech Republic
    'brunei': 'BRN',
    'bruneidarussal': 'BRN',  # Short for Brunei Darussalam
    'bucharest': 'ROU',  # Capital city of Romania
    'bukade': None,  # Ambiguous
    'bulgari': 'BGR',  # Typo for Bulgaria
    'bulgaria': 'BGR',
    'burkinafaso': 'BFA',
    'burundi': 'BDI',
    'busan': 'KOR',  # City in South Korea
    'california': 'USA',  # State in the USA
    'cambodia': 'KHM',
    'cameron': 'CMR',  # Typo for Cameroon
    'cameroon': 'CMR',
    'canada': 'CAN',
    'cananda': 'CAN',  # Typo for Canada
    'canterbury': 'GBR',  # City in the UK
    'capeverde': 'CPV',
    'caymanislands': 'CYM',
    'centafricanre': 'CAF',  # Central African Republic
    'centafrrepubl': 'CAF',  # Central African Republic
    'chad': 'TCD',
    'cheonansi': 'KOR',  # City in South Korea
    'chezrepublic': 'CZE',  # Typo for Czech Republic
    'chile': 'CHL',
    'colombia': 'COL',
    'colorado': 'USA',  # State in the USA
    'comoros': 'COM',
    'congo': 'COG',  # Assuming Republic of the Congo
    'congothedemo': 'COD',  # Democratic Republic of the Congo
    'cookislands': 'COK',
    'costarica': 'CRI',
    'cotedivoire': 'CIV',
    'coteivoire': 'CIV',  # Another name for Côte d'Ivoire
    'crimea': 'UKR',  # Region in Ukraine, though disputed with Russia
    'croatia': 'HRV',
    'cuba': 'CUB',
    'curacao': 'CUW',
    'cyprus': 'CYP',
    'czech': 'CZE',  # Short for Czech Republic
    'czechi': 'CZE',  # Typo for Czech Republic
    'czechrepublic': 'CZE',
    'daegu': 'KOR',  # City in South Korea
    'democraticrepu': 'COD',  # Short for Democratic Republic of the Congo
    'demorepcongo': 'COD',  # Another name for Democratic Republic of the Congo
    'demrepcongo': 'COD',  # Another name for Democratic Republic of the Congo
    'denmark': 'DNK',
    'deutschland': 'DEU',  # Germany in German
    'djibouti': 'DJI',
    'dominica': 'DMA',
    'dominicanrep': 'DOM',  # Dominican Republic
    'drammen': 'NOR',  # City in Norway
    'dusseldorf': 'DEU',  # City in Germany
    'easttimor': 'TLS',  # Timor-Leste
    'ebgland': 'GBR',  # Typo for England, UK
    'ecuador': 'ECU',
    'edinburgh': 'GBR',  # City in the UK
    'egnland': 'GBR',  # Typo for England, UK
    'egypt': 'EGY',
    'elsalvador': 'SLV',
    'emgland': 'GBR',  # Typo for England, UK
    'endland': 'GBR',  # Typo for England, UK
    'eng': 'GBR',  # Short for England, UK
    'engaland': 'GBR',  # Typo for England, UK
    'engalnd': 'GBR',  # Typo for England, UK
    'engand': 'GBR',  # Typo for England, UK
    'enghland': 'GBR',  # Typo for England, UK
    'englad': 'GBR',  # Typo for England, UK
    'engladn': 'GBR',  # Typo for England, UK
    'englalnd': 'GBR',  # Typo for England, UK
    'england': 'GBR',
    'englandvvvvvvvv': 'GBR',  # Typo for England, UK
    'englanf': 'GBR',  # Typo for England, UK
    'englang': 'GBR',  # Typo for England, UK
    'englenad': 'GBR',  # Typo for England, UK
    'english': 'GBR',  # Referring to England, UK
    'englnad': 'GBR',  # Typo for England, UK
    'engnland': 'GBR',  # Typo for England, UK
    'enlgland': 'GBR',  # Typo for England, UK
    'equatguinea': 'GNQ',  # Equatorial Guinea
    'eritrea': 'ERI',
    'estonia': 'EST',
    'eswatini': 'SWZ',
    'ethiopia': 'ETH',
    'eygpt': 'EGY',  # Typo for Egypt
    'falklandisland': 'FLK',  # Falkland Islands
    'fance': 'FRA',  # Typo for France
    'faroeislands': 'FRO',
    'fiji': 'FJI',
    'finland': 'FIN',
    'france': 'FRA',
    'french': 'FRA',  # Referring to France
    'frenchguiana': 'GUF',
    'frenchpolynesi': 'PYF',  # French Polynesia
    'frenchwestind': None,  # Ambiguous, possibly referring to French territories in the West Indies
    'gabon': 'GAB',
    'gambia': 'GMB',
    'geneva': 'CHE',  # City in Switzerland
    'georgia': 'GEO',
    'germany': 'DEU',
    'ghana': 'GHA',
    'gibraltar': 'GIB',
    'goa': 'IND',  # State in India
    'greece': 'GRC',
    'greenland': 'GRL',
    'grenada': 'GRD',
    'guatemala': 'GTM',
    'guinea': 'GIN',
    'guineabissau': 'GNB',
    'guyana': 'GUY',
    'haiti': 'HTI',
    'honduras': 'HND',
    'hungary': 'HUN',
    'iceland': 'ISL',
    'india': 'IND',
    'indonesia': 'IDN',
    'iran': 'IRN',
    'iraq': 'IRQ',
    'ireland': 'IRL',
    'israel': 'ISR',
    'italy': 'ITA',
    'jamaica': 'JAM',
    'japan': 'JPN',
    'jordan': 'JOR',
    'kazakhstan': 'KAZ',
    'kenya': 'KEN',
    'kiribati': 'KIR',
    'kuwait': 'KWT',
    'kyrgyzstan': 'KGZ',
    'laos': 'LAO',
    'latvia': 'LVA',
    'lebanon': 'LBN',
    'lesotho': 'LSO',
    'liberia': 'LBR',
    'libya': 'LBY',
    'liechtenstein': 'LIE',
    'lithuania': 'LTU',
    'luxembourg': 'LUX',
    'madagascar': 'MDG',
    'malawi': 'MWI',
    'malaysia': 'MYS',
    'maldives': 'MDV',
    'mali': 'MLI',
    'malta': 'MLT',
    'marshallisland': 'MHL',  # Marshall Islands
    'mauritania': 'MRT',
    'mauritius': 'MUS',
    'mexico': 'MEX',
    'micronesia': 'FSM',  # Federated States of Micronesia
    'moldova': 'MDA',
    'monaco': 'MCO',
    'mongolia': 'MNG',
    'montenegro': 'MNE',
    'morocco': 'MAR',
    'mozambique': 'MOZ',
    'myanmar': 'MMR',
    'namibia': 'NAM',
    'nauru': 'NRU',
    'nepal': 'NPL',
    'netherlands': 'NLD',
    'newzealand': 'NZL',
    'nicaragua': 'NIC',
    'niger': 'NER',
    'nigeria': 'NGA',
    'northkorea': 'PRK',  # Democratic People's Republic of Korea
    'northmacedonia': 'MKD',
    'norway': 'NOR',
    'oman': 'OMN',
    'pakistan': 'PAK',
    'palau': 'PLW',
    'palestine': 'PSE',
    'panama': 'PAN',
    'papuanguinea': 'PNG',  # Papua New Guinea
    'paraguay': 'PRY',
    'peru': 'PER',
    'philippines': 'PHL',
    'poland': 'POL',
    'portugal': 'PRT',
    'qatar': 'QAT',
    'romania': 'ROU',
    'russia': 'RUS',
    'rwanda': 'RWA',
    'saintkittsand': 'KNA',  # Saint Kitts and Nevis
    'samoa': 'WSM',
    'sanmarino': 'SMR',
    'saudiarabia': 'SAU',
    'senegal': 'SEN',
    'serbia': 'SRB',
    'seychelles': 'SYC',
    'sierraleone': 'SLE',
    'singapore': 'SGP',
    'slovakia': 'SVK',
    'slovenia': 'SVN',
    'solomonislands': 'SLB',
    'somalia': 'SOM',
    'southafrica': 'ZAF',
    'southkorea': 'KOR',  # Republic of Korea
    'southsudan': 'SSD',
    'spain': 'ESP',
    'srilanka': 'LKA',
    'sudan': 'SDN',
    'suriname': 'SUR',
    'sweden': 'SWE',
    'switzerland': 'CHE',
    'syria': 'SYR',
    'taiwan': 'TWN',
    'tajikistan': 'TJK',
    'tanzania': 'TZA',
    'thailand': 'THA',
    'togo': 'TGO',
    'trinidadandto': 'TTO',  # Trinidad and Tobago
    'tunisia': 'TUN',
    'turkey': 'TUR',
    'turkmenistan': 'TKM',
    'tuvalu': 'TUV',
    'uganda': 'UGA',
    'ukraine': 'UKR',
    'unitedarabem': 'ARE',  # United Arab Emirates
    'unitedkingdom': 'GBR',
    'unitedstates': 'USA',
    'uruguay': 'URY',
    'uzbekistan': 'UZB',
    'vanuatu': 'VUT',
    'vatican': 'VAT',
    'venezuela': 'VEN',
    'vietnam': 'VNM',
    'yemen': 'YEM',
    'zambia': 'ZMB',
    'zimbabwe': 'ZWE',
    'frankreich': 'FRA',  # Germany in French
    'frpolynesia': 'PYF',  # French Polynesia
    'fuzhou': 'CHN',  # City in China
    'gemany': 'DEU',  # Typo for Germany
    'german': 'DEU',  # Referring to Germany
    'germay': 'DEU',  # Typo for Germany
    'germnay': 'DEU',  # Typo for Germany
    'gremany': 'DEU',  # Typo for Germany
    'groningen': 'NLD',  # City in the Netherlands
    'grreece': 'GRC',  # Typo for Greece
    'guadalajara': 'MEX',  # City in Mexico
    'guam': 'GUM',
    'guernsey': 'GGY',
    'guildford': 'GBR',  # City in the UK
    'gwangyeogsita': 'KOR',  # Likely referring to a place in South Korea
    'haryana': 'IND',  # State in India
    'helwan': 'EGY',  # City in Egypt
    'hongkongsar': 'HKG',  # Hong Kong Special Administrative Region
    'hsinchu': 'TWN',  # City in Taiwan
    'iindia': 'IND',  # Typo for India
    'indi': 'IND',  # Typo for India
    'indian': 'IND',  # Referring to India
    'indis': 'IND',  # Typo for India
    'ira': 'IRN',  # Typo for Iran
    'iranf': 'IRN',  # Typo for Iran
    'iranislamicr': 'IRN',  # Iran Islamic Republic
    'iranof': 'IRN',  # Typo for Iran
    'iranx': 'IRN',  # Typo for Iran
    'irn': 'IRN',  # Short for Iran
    'isleofman': 'IMN',
    'it': 'ITA',  # Short for Italy
    'itlay': 'ITA',  # Typo for Italy
    'itly': 'ITA',  # Typo for Italy
    'jakarta': 'IDN',  # Capital city of Indonesia
    'japann': 'JPN',  # Typo for Japan
    'jersey': 'JEY',
    'kambodscha': 'KHM',  # German name for Cambodia
    'kansas': 'USA',  # State in the USA
    'karnataka': 'IND',  # State in India
    'kazakhtan': 'KAZ',  # Typo for Kazakhstan
    'keelungcity': 'TWN',  # City in Taiwan
    'kentucky': 'USA',  # State in the USA
    'kerala': 'IND',  # State in India
    'kinshasa': 'COD',  # City in Democratic Republic of the Congo
    'kobe': 'JPN',  # City in Japan
    'konstanz': 'DEU',  # City in Germany
    'korearepublic': 'KOR',  # Republic of Korea
    'kosovo': 'XKX',  # Provisional code for Kosovo
    'ksouthkorea': 'KOR',  # Typo for South Korea
    'kurdistan': None,  # Kurdistan is a region spanning several countries
    'lagos': 'NGA',  # City in Nigeria
    'lancaster': 'GBR',  # City in the UK
    'lao': 'LAO',  # Lao People's Democratic Republic
    'laodemocratic': 'LAO',  # Lao People's Democratic Republic
    'laopeoplesde': 'LAO',  # Lao People's Democratic Republic
    'limerick': 'IRL',  # City in Ireland
    'lincoln': 'GBR',  # City in the UK
    'lisbon': 'PRT',  # Capital city of Portugal
    'liverpool': 'GBR',  # City in the UK
    'loas': 'LAO',  # Typo for Lao People's Democratic Republic
    'loughborough': 'GBR',  # Town in the UK
    'lucknow': 'IND',  # City in India
    'luneburg': 'DEU',  # City in Germany
    'luzhou': 'CHN',  # City in China
    'macau': 'MAC',  # Macao Special Administrative Region
    'macausarchin': 'MAC',  # Typo for Macao Special Administrative Region
    'manchester': 'GBR',  # City in the UK
    'martinique': 'MTQ',
    'melbourne': 'AUS',  # City in Australia
    'mexixo': 'MEX',  # Typo for Mexico
    'michigan': 'USA',  # State in the USA
    'mixico': 'MEX',  # Typo for Mexico
    'montserrat': 'MSR',
    'nanjing': 'CHN',  # City in China
    'nethantilles': 'ANT',  # Netherlands Antilles (obsolete since 2010)
    'netherland': 'NLD',  # Netherlands
    'netherlandspubl': 'NLD',  # Netherlands
    'neuherberg': 'DEU',  # Location in Germany
    'newcaledonia': 'NCL',
    'newmexico': 'USA',  # State in the USA
    'newyork': 'USA',  # State in the USA
    'nigeriaar': 'NGA',  # Typo for Nigeria
    'nigeriahow': 'NGA',  # Typo for Nigeria
    'nmacedonia': 'MKD',  # North Macedonia
    'northcyprus': None,  # Northern Cyprus is a self-declared state recognized only by Turkey
    'northerncyprus': None,  # Same as above
    'northernireland': 'GBR',  # Part of the UK
    'northernisland': 'GBR',  # Typo for Northern Ireland, part of the UK
    'northireland': 'GBR',  # Typo for Northern Ireland, part of the UK
    'northsudan': 'SDN',  # Sudan (South Sudan is the southern part)
    'nottingham': 'GBR',  # City in the UK
    'nugegoda': 'LKA',  # City in Sri Lanka
    'okayama': 'JPN',  # City in Japan
    'pa': 'USA',  # Pennsylvania, a state in the USA
    'pakisatn': 'PAK',  # Typo for Pakistan
    'palawan': 'PHL',  # Island in the Philippines
    'palestin': 'PSE',  # Palestine
    'palestinestat': 'PSE',  # Palestine
    'palestinian': 'PSE',  # Palestine
    'paris': 'FRA',  # Capital city of France
    'peaplesrchine': 'CHN',  # Typo for People's Republic of China
    'pennsylvania': 'USA',  # State in the USA
    'peolpesrchina': 'CHN',  # Typo for People's Republic of China
    'peopelsrchina': 'CHN',  # Typo for People's Republic of China
    'peopesrchina': 'CHN',  # Typo for People's Republic of China
    'peoplerchina': 'CHN',  # Typo for People's Republic of China
    'peopleschina': 'CHN',  # Typo for People's Republic of China
    'peoplesrchin': 'CHN',  # Typo for People's Republic of China
    'peoplesrchina': 'CHN',  # People's Republic of China
    'peoplesrepubl': 'CHN',  # People's Republic of China
    'peoplesrepubli': 'CHN',  # People's Republic of China
    'peoplesrrepub': 'CHN',  # Typo for People's Republic of China
    'peoplsrchina': 'CHN',  # Typo for People's Republic of China
    'peplesrchina': 'CHN',  # Typo for People's Republic of China
    'philipines': 'PHL',  # Typo for Philippines
    'phillippines': 'PHL',  # Typo for Philippines
    'phlilippines': 'PHL',  # Typo for Philippines
    'poplesrchina': 'CHN',  # Typo for People's Republic of China
    'portauprince': 'HTI',  # Capital city of Haiti
    'portondown': 'GBR',  # Location in the UK
    'prchina': 'CHN',  # People's Republic of China
    'pretoria': 'ZAF',  # City in South Africa
    'proplesrchina': 'CHN',  # Typo for People's Republic of China
    'puebla': 'MEX',  # City in Mexico
    'puertorico': 'PRI',  # Puerto Rico
    'punjab': 'IND',  # State in India
    'qata': 'QAT',  # Typo for Qatar
    'reland': 'IRL',  # Typo for Ireland
    'repcongo': 'COG',  # Republic of the Congo
    'republdominica': 'DOM',  # Typo for Dominican Republic
    'republicofarm': 'ARM',  # Republic of Armenia
    'republicofkor': 'KOR',  # Republic of Korea
    'republicofkos': 'XKX',  # Republic of Kosovo (provisional code)
    'republicofnor': 'MKD',  # Republic of North Macedonia
    'reunion': 'REU',
    'rj': 'BRA',  # Rio de Janeiro, a state in Brazil
    'rmany': 'DEU',  # Typo for Germany
    'sainthelena': 'SHN',  # Saint Helena
    'saintkittsn': 'KNA',  # Saint Kitts and Nevis
    'santodomingo': 'DOM',  # Capital city of Dominican Republic
    'saopaulo': 'BRA',  # City in Brazil
    'saotomeandpr': 'STP',  # Sao Tome and Principe
    'saotomeprin': 'STP',  # Sao Tome and Principe
    'sapin': 'ESP',  # Typo for Spain
    'saudiarabiam': 'SAU',  # Typo for Saudi Arabia
    'saudiarbia': 'SAU',  # Typo for Saudi Arabia
    'scotaland': 'GBR',  # Typo for Scotland, part of the UK
    'scotalnd': 'GBR',  # Typo for Scotland, part of the UK
    'scotland': 'GBR',  # Part of the UK
    'seeb': 'OMN',  # City in Oman
    'seoulkorea': 'KOR',  # Seoul, capital city of South Korea
    'shanghai': 'CHN',  # City in China
    'shankaraghatta': 'IND',  # Place in India
    'sheffield': 'GBR',  # City in the UK
    'shenyang': 'CHN',  # City in China
    'sikkim': 'IND',  # State in India
    'sinagapore': 'SGP',  # Typo for Singapore
    'sintmaarten': 'SXM',
    'slovenija': 'SVN',  # Slovenian name for Slovenia
    'soithkorea': 'KOR',  # Typo for South Korea
    'somaliland': 'SOM',  # Self-declared state, internationally recognized as part of Somalia
    'soouthkorea': 'KOR',  # Typo for South Korea
    'soputhkorea': 'KOR',  # Typo for South Korea
    'sothkorea': 'KOR',  # Typo for South Korea
    'sotuhkorea': 'KOR',  # Typo for South Korea
    'sou': None,  # Ambiguous
    'souhkorea': 'KOR',  # Typo for South Korea
    'soulkorea': 'KOR',  # Typo for Seoul, South Korea
    'souothkorea': 'KOR',  # Typo for South Korea
    'south': None,  # Ambiguous
    'southatlantic': None,  # Ambiguous region
    'southaustralia': 'AUS',  # State in Australia
    'southeastafric': None,  # Ambiguous region
    'southkoea': 'KOR',  # Typo for South Korea
    'southkora': 'KOR',  # Typo for South Korea
    'southkore': 'KOR',  # Typo for South Korea
    'southkoreare': 'KOR',  # Typo for South Korea
    'southkoreasch': 'KOR',  # Typo for South Korea
    'souuthkorea': 'KOR',  # Typo for South Korea
    'soythkorea': 'KOR',  # Typo for South Korea
    'spainj': 'ESP',  # Typo for Spain
    'srilanaka': 'LKA',  # Typo for Sri Lanka
    'standrews': 'GBR',  # City in the UK
    'sthelena': 'SHN',  # Saint Helena
    'stkittsnevi': 'KNA',  # Saint Kitts and Nevis
    'stlucia': 'LCA',
    'stmartin': 'MAF',
    'stvincent': 'VCT',  # Saint Vincent and the Grenadines
    'suadiarabia': 'SAU',  # Typo for Saudi Arabia
    'suthkorea': 'KOR',  # Typo for South Korea
    'suwon': 'KOR',  # City in South Korea
    'svalbard': 'SJM',  # Svalbard and Jan Mayen
    'swedan': 'SWE',  # Typo for Sweden
    'switzerlandins': 'CHE',  # Typo for Switzerland
    'sydney': 'AUS',  # City in Australia
    'syrian': 'SYR',  # Referring to Syria
    'tahiti': 'PYF',  # Part of French Polynesia
    'taipei': 'TWN',  # City in Taiwan
    'taiwa': 'TWN',  # Typo for Taiwan
    'taiwam': 'TWN',  # Typo for Taiwan
    'taiwana': 'TWN',  # Typo for Taiwan
    'taiwanprovinc': 'TWN',  # Taiwan Province
    'taiwanprovince': 'TWN',  # Taiwan Province
    'taiwanx': 'TWN',  # Typo for Taiwan
    'taly': 'ITA',  # Typo for Italy
    'tanzaniaunite': 'TZA',  # United Republic of Tanzania
    'tanzaniax': 'TZA',  # Typo for Tanzania
    'tanzanta': 'TZA',  # Typo for Tanzania
    'thenetherlands': 'NLD',  # The Netherlands
    'tianjin': 'CHN',  # City in China
    'timorleste': 'TLS',  # East Timor
    'tonga': 'TON',
    'trinidadtobago': 'TTO',  # Trinidad and Tobago
    'tristandacunh': 'SHN',  # Tristan da Cunha, part of Saint Helena, Ascension and Tristan da Cunha
    'trukey': 'TUR',  # Typo for Turkey
    'tschechien': 'CZE',  # German name for Czech Republic
    'tubingen': 'DEU',  # City in Germany
    'turksandcaico': 'TCA',  # Turks and Caicos Islands
    'turkscaicos': 'TCA',  # Turks and Caicos Islands
    'twaian': 'TWN',  # Typo for Taiwan
    'uarabemira': 'ARE',  # United Arab Emirates
    'uarabemiratas': 'ARE',  # Typo for United Arab Emirates
    'uarabemirate': 'ARE',  # Typo for United Arab Emirates
    'uarabemirates': 'ARE',  # United Arab Emirates
    'ukengland': 'GBR',  # England, part of the UK
    'ukscotland': 'GBR',  # Scotland, part of the UK
    'uniatedarabem': 'ARE',  # Typo for United Arab Emirates
    'unitedstateso': 'USA',  # Typo for United States of America
    'univ': None,  # Ambiguous, possibly referring to a university
    'univglasgow': 'GBR',  # University of Glasgow, located in the UK
    'us': 'USA',  # United States of America
    'usa': 'USA',  # United States of America
    'usas': 'USA',  # Typo for United States of America
    'uzbekisktan': 'UZB',  # Typo for Uzbekistan
    'venezuelaboli': 'VEN',  # Venezuela, Bolivarian Republic of
    'vereinigteskon': 'DEU',  # German for "United Kingdom"
    'vienna': 'AUT',  # Capital city of Austria
    'virginislands': 'VIR',  # U.S. Virgin Islands
    'waales': 'GBR',  # Typo for Wales, part of the UK
    'waes': 'GBR',  # Typo for Wales, part of the UK
    'wales': 'GBR',  # Part of the UK
    'washington': 'USA',  # State in the USA
    'westbengal': 'IND',  # State in India
    'westindies': None,  # Refers to a region, not a specific country
    'wlaes': 'GBR',  # Typo for Wales, part of the UK
    'yenagoa': 'NGA',  # City in Nigeria
    'zaragoza': 'ESP',  # City in Spain
    'zollikerberg': 'CHE',  # Location in Switzerland
    'hina': 'CHN',
    'peolplesrchin': 'CHN',
    'lianyungang': 'CHN',
    'tokelauislands': 'NZL',
    'ascensionisl': 'GBR',
    'macedonia': 'MKD',
    'swaziland':'SWZ',
    'czechoslovakia': 'CSK',
    'hongkong': 'HKG',
    'senegambia': 'SEN',
    'ussr': 'SUN',
    'yugoslavia' : 'YUG' ,
    'repofgeorgia': 'GEO',  # Georgia
    'windassocst': None,  # No specific disambiguation provided
    'zaire': 'COD',  # Democratic Republic of the Congo
    'ciskei': 'CIS',  # Former apartheid-era homeland in South Africa
    'korea': 'KOR',  # South Korea
    'malagasyrepubl': 'MDG',  # Madagascar
    'girona': 'ESP',  # City in Spain
    'serbiamonteneg': 'SRB',  # Former state union of Serbia and Montenegro
    'niue': 'NIU',  # Island nation of Niue
    'falklandisl': 'FLK',  # Falkland Islands
    'janmayen': 'SJM',  # Island of Jan Mayen
    'bendigo': 'AUS',  # City in Australia
    'merseysi': 'GBR',  # Region in the United Kingdom
    'trinidtobago': 'TTO',  # Trinidad and Tobago
    'britindocean': 'IOT',  # British Indian Ocean Territory
    'morocca': 'MAR',  # Morocco
    'stbarthelemy': 'BLM',  # Saint Barthélemy
    'arizonaliverh': 'USA',  # Arizona Liver Health
    'clalithlthser': 'CHN',  # Unknown, no specific disambiguation provided
    'peolesrchina': 'CHN',  # People's Republic of China
    'princeton': 'USA',  # University in USA
    'francee': 'FRA',  # France
    'messina': 'ITA',  # City in Italy
    'panamacity': 'PAN',  # Panama City
    'shouthkorea': 'KOR',  # South Korea
    'sultanateofom': 'OMN',  # Sultanate of Oman
    'pepolesrchina': 'CHN'
}

# note, gpt 4, o3-mini-high used to create above disambiguation list

# base directory: current working directory (run script in the folder with your CSVs)
base_output_directory = os.path.dirname(os.path.abspath(__file__))

# define categories based on your file naming convention
categories = [
    'arts_humanities',
    'science_technology',
    'socialsciences'
]

def process_and_save_csv(input_file: str, output_file: str) -> None:
    """
    Load a country co-occurrence CSV, apply disambiguation mappings,
    consolidate duplicate country codes, and write out a new CSV.
    """
    df = pd.read_csv(input_file, index_col=0)

    # standardized renaming in one go
    df = df.rename(index=disambiguation_dict, columns=disambiguation_dict)

    # drop any rows/columns mapped to None
    df = df.drop(index=[None], columns=[None], errors='ignore')
    df = df.loc[:, ~df.columns.str.contains(r'^Unnamed')]

    # consolidate duplicates along both axes
    df_grouped = (
        df.groupby(level=0, axis=1)
          .sum()
          .groupby(level=0, axis=0)
          .sum()
    )

    df_grouped.to_csv(output_file)
    logging.info('Saved disambiguated CSV: %s', output_file)


def main() -> None:
    """
    Iterate over years and categories, processing each CSV if present.
    """
    for category in categories:
        for year in range(1993, 2023):
            input_filename = f'country_matrix_{year}_{category}.csv'
            output_filename = f'country_matrix_{year}_{category}_dis.csv'
            input_path = os.path.join(base_output_directory, input_filename)
            output_path = os.path.join(base_output_directory, output_filename)

            if not os.path.isfile(input_path):
                logging.warning('Missing file, skipping: %s', input_path)
                continue

            logging.info('Processing file: %s', input_path)
            process_and_save_csv(input_path, output_path)


if __name__ == '__main__':
    main()
