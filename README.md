# Haskell server for IRCMQ by IRCMQ boys

Server side of the project creating the Subscriber and Publisher sockets for zeromq. Takes in a message from the client in a JSON format. 

## Features

| Feature | Implemented |
|--|--|
| Hello message when a user enters a channel | ✔ |
| Goodbye message when a user leaves a channel | ✔ |
| Acknowledges a received message. This gets passed to all clients in that channel | ✔ |
| List of all channels participants | ✔ |

## Testing

For this project, some tests where written for the JSON parsing implemented. Run command:
```
stack test
```

Implementing further tests on the other functionality was difficult due to the fact that this is a chat application, which entails the usage of IO in Haskell. Because our entire "business logic" (on the server side) is about handling the different server actions, and because we are using multiple different sockets, we had to use MVars (Monadic variables) for handling our "states". MVars is also wrapped in IO and this lead the entire server application to be wrapped in IO. Reflecting on this, we would say that Haskell might not be the optimal language to use for this kind of server program. 

## Instructions

**Clone the repo into a clean directory**

```
git clone https://git.gvk.idi.ntnu.no/course/prog2006/as/denniskr/ircmq-boys/server-haskell.git
```

### Installation - Windows

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

**Specific instructions for ZeroMQ on Windows - Curtesy Dennis Kristiansen**

https://git.gvk.idi.ntnu.no/course/prog2006/prog2006-2021/-/issues/40

### Installation - Linux

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

### Installation - MacOS

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

### Documentation

Haddock has been used as the documentation standard for the program. With stack installed, you have this available for building by running (inside the project directory):

```
stack haddock
```

This will build the documentation in the .stack-work folder, most likely in the dist folder followed by the doc folder. You can open the <filename>.html in there to view the documentation of the <filename>.hs file.
