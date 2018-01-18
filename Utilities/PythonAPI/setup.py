from setuptools import setup, find_packages
from os import path
basepath = path.dirname(path.abspath(__file__))
root = path.abspath(path.join(basepath,'..','..'))
setup(
    name='esme',
    packages=find_packages(),
    url='https://esme.bu.edu',
    entry_points={
        'console_scripts': [
            'esme = esme.__main__:main'
        ]
    },
    license='',
    setup_requires=['vcversioner'],
    vcversioner={'version_module_paths': ['esme/_version.py'],
                 'root': root,
                 'version_file': 'python_api_version.txt'
                 },
    author='Graham Voysey',
    author_email='gvoysey@bu.edu',
    description='A python API for reading ESME Workbench simulation results',
    install_requires=['attrs>=17.4.0']
)
