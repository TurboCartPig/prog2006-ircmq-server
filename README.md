# Haskell server for IRCMQ by IRCMQ boys

Handles the server side of the project creating the Subscriber and Publisher sockets for zeromq. Takes in a message from the client in a JSON format. 

## Features

[_] - Hello message when a user enters a channel

[_] - Goodbye message when a user leaves a channel

[_] - Acknowledges a recieved message. This gets passed to all clients in that channel

[_] - List of all channels participants

## Instructions

**Clone the repo into a clean directory**

```
git clone https://git.gvk.idi.ntnu.no/course/prog2006/as/denniskr/ircmq-boys/server-haskell.git
```

### Installation and run instructions - Windows

**source:** https://www.haskell.org/platform/windows.html

**sourde:** https://chocolatey.org/install

- Download and configure Chocolatey
  - Using administrative shell
  - Copy and run the following command
    ```
      Set-ExecutionPolicy Bypass -Scope Process -Force; [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))
    ```
  - Paste it into your shell and press Enter
  - If no errors appear, you are ready to use **Chocolatey**. Test it with `choco` command in the shell.
- If you are upgrading from the old-style haskell-platform installer, clean the cabal configuration by running

  ```
  cabal user-config init -f
  ```

- At an elevated command prompt, run
  ```
  choco install haskell-dev
  refreshenv
  ```
  **To install Stack**
- Download the installer from
  ```
  https://get.haskellstack.org/stable/windows-x86_64-installer.exe
  ```

### Installation instructions - Linux

**Clone the repo into a clean directory**

source: https://www.haskell.org/platform/linux.html

**Ubuntu**

- In the terminal, run
  ```
  sudo apt-get install haskell-platform
  ```

**Redhat**

- In the terminal, run
  ```
  sudo yum install haskell-platform
  ```

**Fedora**

- In the terminal, run
  ```
  sudo dnf install haskell-platform
  ```

**To install Stack**

- In the terminal, run
  ```
  curl -sSL https://get.haskellstack.org/ | sh
  ```
  or:
  ```
  wget -qO- https://get.haskellstack.org/ | sh
  ```

### Installation instructions - MacOS

**Clone the repo into a clean directory**

source: https://docs.haskellstack.org/en/stable/README/

- To install, run and follow the onscreen instructions
  ```
  curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
  ```

**To install Stack**

- If you already have a version of stack installed, run

  ```
  stack upgrade
  ```

- Run the following command in the terminal
  ```
  curl -sSL https://get.haskellstack.org/ | sh
  ```
  or:
  ```
  wget -qO- https://get.haskellstack.org/ | sh
  ```



### Run the server

**Inside the project directory**

* Run the program

```
stack run
```

* Run the tests

```
stack test
```

### Documentation

Haddock has been used as the documentation standard for the program. With stack installed, you have this available for building by running (inside the project directory):

```
stack haddock
```

This will build the documentation in the .stack-work folder, most likely in the dist folder followed by the doc folder. You can open the <filename>.html in there to view the documentation of the <filename>.hs file.
