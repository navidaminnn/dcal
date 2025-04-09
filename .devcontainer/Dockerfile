FROM ubuntu:latest

# https://code.visualstudio.com/remote/advancedcontainers/add-nonroot-user#_change-the-uidgid-of-an-existing-container-user
ARG USERNAME=ubuntu
ARG USER_UID=1000
ARG USER_GID=$USER_UID

RUN groupmod --gid $USER_GID $USERNAME \
    && usermod --uid $USER_UID --gid $USER_GID $USERNAME \
    && chown -R $USER_UID:$USER_GID /home/$USERNAME

RUN apt-get update \
    && apt-get install -y openjdk-21-jdk openjdk-21-source git build-essential curl

RUN mkdir -p /home/$USERNAME \
    && chown $USERNAME /home/$USERNAME \
    && su --login $USERNAME -c "curl -sSLf https://scala-cli.virtuslab.org/get | sh" \
    && su --login $USERNAME -c "scala-cli version"

USER $USERNAME
