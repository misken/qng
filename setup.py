from setuptools import find_packages, setup

setup(
    name='qng',
    packages=find_packages("src"),
    package_dir={"": "src"},
    version='0.1.0',
    description='Library of queueing functions',
    author='misken',
    license='MIT',
    install_requires=[
        'scipy',
    ],
)
