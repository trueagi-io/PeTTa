import os
import shutil

from setuptools import setup
from setuptools.command.build_py import build_py

HERE = os.path.abspath(os.path.dirname(__file__))

# Runtime resources living outside the package that must ship inside the wheel,
# mapped to their destination under petta/_runtime/ (preserving the src/ and
# lib/ sibling layout that metta.pl relies on for library_path).
RUNTIME_RESOURCES = {
    "src": "src",
    "lib": "lib",
    os.path.join("python", "helper.pl"): os.path.join("python", "helper.pl"),
}


class build_py_with_runtime(build_py):
    def run(self):
        super().run()
        runtime_root = os.path.join(self.build_lib, "petta", "_runtime")
        for src_rel, dst_rel in RUNTIME_RESOURCES.items():
            src = os.path.join(HERE, src_rel)
            dst = os.path.join(runtime_root, dst_rel)
            if os.path.isdir(src):
                shutil.copytree(src, dst, dirs_exist_ok=True)
            else:
                os.makedirs(os.path.dirname(dst), exist_ok=True)
                shutil.copy2(src, dst)


setup(
    name="petta",
    version="0.1.0",
    packages=["petta"],
    package_dir={"": "python"},
    include_package_data=True,
    cmdclass={"build_py": build_py_with_runtime},
    entry_points={"console_scripts": ["petta=petta.cli:main"]},
    install_requires=[
        'janus-swi',
    ],
    author="Your Name",
    author_email="your.email@example.com",
    description="A Python wrapper for MeTTa",
    long_description=open("README.md").read(),
    long_description_content_type="text/markdown",
    url="https://github.com/your-repo-url",  # Replace with actual repo URL
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: MIT License",  # Adjust based on LICENSE
        "Operating System :: OS Independent",
    ],
    python_requires=">=3.8",
)
