from setuptools import setup, find_packages

setup(
    name="ff3",
    version="0.1.0",
    description="FF3 Format Preserving Encryption - Educational Implementation",
    long_description=open("README.md").read(),
    long_description_content_type="text/markdown",
    packages=find_packages(where="src"),
    package_dir={"": "src"},
    python_requires=">=3.10",
    install_requires=[
        "cryptography>=3.0.0",
    ],
    extras_require={
        "dev": [
            "pytest>=6.0.0",
            "pytest-cov>=3.0.0",
            "pytest-benchmark>=3.0.0",
            "black>=22.0.0",
            "flake8>=4.0.0",
        ]
    },
    entry_points={
        "console_scripts": [
            "ff3-cli=cli.ff3_cli:main",
            "ff3-validate=cli.ff3_validate:main",
            "ff3-benchmark=cli.ff3_benchmark:main",
            "ff3-stresstest=cli.ff3_stresstest:main",
        ]
    },
    license="BUSL-1.1",
)
