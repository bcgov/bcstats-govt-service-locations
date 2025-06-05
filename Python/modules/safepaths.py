# Copyright 2025 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# This script loads raw csv data files containing spatial data for addresses in 
# a defined municipality in BC (loc).  The datasets are processed to be ready 
# for use for data analytics. 

import os
from dotenv import load_dotenv
load_dotenv()

def use_network_path(*args):
  """
  Constructs a file path relative to the network path set with `set_network_path()`.
  Takes as arguments:
    *args: One or more character strings representing path components.
  Returns:
    The full file path.
  Raises:
    ValueError: If any argument is not a string or if the root network path does not exist.
  """
  x = list(args)
  
  root = get_network_path()

  if not x:
    return root

  if not all(isinstance(item, str) for item in x):
    raise ValueError("x must be a character string denoting a path relative to the network path set with `set_network_path()`.")

  x = os.path.join(*x)
 
  return os.path.join(root, x.lstrip("/"))


def get_network_path():
  """
  Retrieves the network path from the environment variable.
  Returns:
    The network path as a string.
  """

  network_path = os.getenv(path_envvar_name()) # Assuming path_envvar_name is defined elsewhere.
  safepaths_sitrep() # Assuming safepaths_sitrep is defined elsewhere.
  return network_path


def set_network_path(x):
  """
  Sets the network path in the os.environ environment variables.
  Takes as arguments:
    x: The network path to set.
  Returns:
    True if the path was set successfully, False otherwise (if overwrite is declined).
  """
  path_env_var = path_envvar_name()

  if path_env_var in os.environ:
    overwrite = input(f"Your network path has already been set. Overwrite? (y/n): ").lower()
    if overwrite != "y":
      return False

  os.environ[path_env_var] = x

  print(f"Network path '{path_env_var}' set to: {x}")
  return True

def check_if_available():
  """
  Checks if the directory specified by the environment variable exists.
  Raises an error if it doesn't.
  """
  network_path = os.getenv(path_envvar_name()) #Assuming path_envvar_name is defined elsewhere.
  if not os.path.exists(network_path):
    raise ValueError("There has been a problem. Are you sure you are connected to the VPN?")

# Example placeholder for path_envvar_name(), replace with actual implementation.
def path_envvar_name():
  """
  Returns the name of the environment variable containing the network path.
  """
  return "SAFEPATHS_NETWORK_PATH" # Example, replace with the actual name.

def safepaths_sitrep():
  """
  Performs safety checks and reports the status of the safepath.
  """
  check_if_set() # Assuming check_if_set is defined elsewhere.
  check_if_available() # Assuming check_if_available is defined elsewhere.
  print("safepath set and accessible") # Equivalent to R's message()
  return True # Equivalent to R's invisible(TRUE)


def check_if_set():
  """
  Checks if the network path environment variable is set.
  Raises an error if it's empty.
  """
  network_path = os.getenv(path_envvar_name()) # Assuming path_envvar_name is defined elsewhere.
  if not network_path: # Pythonic way to check for empty strings
    raise ValueError("You need to set your network path. Use set_network_path()")