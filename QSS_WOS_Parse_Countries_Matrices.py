from collections import defaultdict
import pandas as pd
from concurrent.futures import ProcessPoolExecutor
import os
import xml.etree.ElementTree as ET
import re
import numpy as np
import gzip
from itertools import combinations

def standardize_country_name(name):
    """Standardize the country name by removing non-alphabetical characters and converting to lowercase."""
    return re.sub('[^a-zA-Z]', '', name).lower()

def process_single_file(xml_file, category):
    """Process a single XML file to extract country and country pair information."""
    print(f"Starting XML parsing for {xml_file}...")
    all_countries = set()
    country_pairs = defaultdict(int)
    ns = {'ns': 'http://clarivate.com/schema/wok5.30/public/FullRecord'}

    category_mapping = {
        'Science & Technology': 'Science & Technology',
        'Arts & Humanities': 'Arts & Humanities',
        'Social Sciences': 'Social Sciences'
    }

    try:
        with gzip.open(xml_file, 'rt') as file:
            for event, elem in ET.iterparse(file):
                if elem.tag.endswith('REC'):
                    headings_elem = elem.findall('.//ns:headings/ns:heading', ns)
                    if any(heading.text == category_mapping[category] for heading in headings_elem):
                        addresses_elem = elem.find('.//ns:addresses', ns)
                        if addresses_elem is not None and int(addresses_elem.attrib.get('count', 0)) > 1:
                            countries = set()
                            for address_name in addresses_elem.findall('ns:address_name', ns):
                                country_elem = address_name.find('./ns:address_spec/ns:country', ns)
                                if country_elem is not None:
                                    country_name = standardize_country_name(country_elem.text)
                                    countries.add(country_name)
                            all_countries.update(countries)
                            for c1, c2 in combinations(sorted(countries), 2):
                                country_pairs[(c1, c2)] += 1
                    elem.clear()
    except Exception as e:
        print(f"An error occurred during XML parsing: {e}")

    print(f"Finished XML parsing for {xml_file}. Found {len(all_countries)} countries and {len(country_pairs)} country pairs.")
    return all_countries, country_pairs

def find_xml_files(directory):
    """Find XML.gz files in the given directory, ignoring the unique identifier part of the filename."""
    return [os.path.join(directory, file) for file in os.listdir(directory) if file.endswith('.xml.gz')]

def process_files_by_category(year_directory, category, year):
    """Process XML files for a specific category and year."""
    xml_files = find_xml_files(year_directory)
    all_countries = set()
    country_pairs = defaultdict(int)

    with ProcessPoolExecutor(max_workers=os.cpu_count()) as executor:
        futures = [executor.submit(process_single_file, xml_file, category) for xml_file in xml_files]
        for future in futures:
            chunk_countries, chunk_pairs = future.result()
            all_countries.update(chunk_countries)
            for key, value in chunk_pairs.items():
                country_pairs[key] += value

    print(f"Total countries found for {category} in {year}: {len(all_countries)}")
    print(f"Total country pairs found for {category} in {year}: {len(country_pairs)}")

    n = len(all_countries)
    matrix = np.zeros((n, n), dtype=int)
    countries_list = sorted(list(all_countries))
    index_mapping = {country: i for i, country in enumerate(countries_list)}

    for (country1, country2), count in country_pairs.items():
        idx1 = index_mapping[country1]
        idx2 = index_mapping[country2]
        matrix[idx1, idx2] += count
        matrix[idx2, idx1] += count

    df = pd.DataFrame(matrix, index=countries_list, columns=countries_list)
    output_filename = f"country_matrix_{year}_{category.replace(' & ', '_').replace(' ', '').lower()}.csv"
    df.to_csv(output_filename)

if __name__ == '__main__':
    base_directory = 'C:/Users/twhetsell3/WOS_XML/ALL_WOS_XML'
    categories = ['Science & Technology', 'Arts & Humanities', 'Social Sciences']

    # loop over the years from 1993 to 2023
    for year in range(1993, 2023):
        year_directory = os.path.join(base_directory, str(year))
        for category in categories:
            process_files_by_category(year_directory, category, year)
