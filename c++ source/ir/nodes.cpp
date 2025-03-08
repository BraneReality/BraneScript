
#include "nodes.h"

bool WeakNodeHandle::expired() const 
{
    return _value.expired() || _inner.expired();
}


std::optional<NodeHandle<IRNode>> WeakNodeHandle::upgrade() const
{
    auto value = _value.lock();
    auto inner = _inner.lock();
    if (value && inner)
        return std::make_optional(NodeHandle<IRNode>(value, inner)); return std::nullopt;
}
