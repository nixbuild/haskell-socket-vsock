{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeFamilies #-}

module System.Socket.Family.VSock
  ( VSock
  , VSockCid
  , VSockPort
  , socketAddressVSock
  , vsockCidAny
  , vsockPortAny
  , vsockCid
  , vsockPort
  ) where

import "base" Data.Word
import "base" Foreign.Marshal.Utils (fillBytes)
import "base" Foreign.Storable
import "socket" System.Socket

#include "sys/socket.h"
#include "linux/vm_sockets.h"

data VSock

type VSockCid = #type unsigned int

type VSockPort = #type unsigned int

instance Family VSock where
  familyNumber _ = (#const AF_VSOCK)
  data SocketAddress VSock = SocketAddressVSock VSockPort VSockCid
    deriving (Eq, Show)

instance Protocol VSock where
  protocolNumber _ = 0

socketAddressVSock :: VSockPort -> VSockCid -> SocketAddress VSock
socketAddressVSock = SocketAddressVSock

instance Storable (SocketAddress VSock) where
  sizeOf    _ = (#size struct sockaddr_vm)
  alignment _ = (#alignment struct sockaddr_vm)

  peek ptr = do
    port <- (#peek struct sockaddr_vm, svm_port) ptr
    cid <- (#peek struct sockaddr_vm, svm_cid) ptr
    pure $ SocketAddressVSock port cid

  poke ptr (SocketAddressVSock port cid) = do
    fillBytes ptr 0 (#const sizeof(struct sockaddr_vm))
    (#poke struct sockaddr_vm, svm_family) ptr ((#const AF_VSOCK) :: Word16)
    (#poke struct sockaddr_vm, svm_port) ptr port
    (#poke struct sockaddr_vm, svm_cid) ptr cid

vsockCidAny :: VSockCid
vsockCidAny = (#const VMADDR_CID_ANY)

vsockPortAny :: VSockCid
vsockPortAny = (#const VMADDR_PORT_ANY)

vsockCid :: SocketAddress VSock -> VSockCid
vsockCid (SocketAddressVSock _ cid) = cid

vsockPort :: SocketAddress VSock -> VSockPort
vsockPort (SocketAddressVSock port _) = port
