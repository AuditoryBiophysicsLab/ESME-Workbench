from setuptools import setup, find_packages

setup(
    name='esme',
    version='',
    packages=find_packages(),
    url='https://esme.bu.edu',
    entry_points={
        'console_scripts': [
            'esme = esme.__main__:main'
        ]
    },
    license='',
    author='Graham Voysey',
    author_email='gvoysey@bu.edu',
    description='A python API for reading ESME Workbench simulation results'
)
