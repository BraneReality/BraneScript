#ifndef BRANESCRIPT_NODES_H
#define BRANESCRIPT_NODES_H

#include <memory>
#include <vector>
#include <optional>
#include <type_traits>

struct IRNode {
    virtual ~IRNode() = default;
};

template<class T>
class NodeHandle;

template<class T>
class NodeRef;

struct NodeHandleInner;

class WeakNodeHandle
{
    std::weak_ptr<IRNode> _value;
    std::weak_ptr<NodeHandleInner> _inner;
public:
    template<class T>
    WeakNodeHandle(NodeHandle<T>& handle): 
        _value(handle._value), 
        _inner(handle._inner)
    {
        static_assert(std::is_base_of<IRNode, T>());
    }
    
    bool expired() const;
    std::optional<NodeHandle<IRNode>> upgrade() const;
};

using NodeHandleRefList = std::vector<std::pair<size_t, WeakNodeHandle>>;

struct NodeHandleInner
{
    /// External References
    NodeHandleRefList _extRefs;
    /// References to this node that are children
    NodeHandleRefList _childRefs;
    size_t _refIdCounter = 0;

    NodeHandleInner() = default;
};

template<class T>
class NodeHandle
{
    std::shared_ptr<T> _value;
    std::shared_ptr<NodeHandleInner> _inner;

    template<class C>
    std::optional<NodeHandle<C>> as()
    {
        std::shared_ptr<C> cValue = std::dynamic_pointer_cast<C>(_value);
        if (cValue == nullptr)
            return std::nullopt;
        return std::make_optional(NodeHandle{
            cValue,
            _inner
        });
    }

    friend class WeakNodeHandle;
    friend class NodeRef<T>;
    NodeHandle(std::shared_ptr<T> value, std::shared_ptr<NodeHandleInner> inner):
        _value(std::move(_value)), _inner(std::move(inner))
    {
    }
    
public:

    WeakNodeHandle downgrade()
    {
        return WeakNodeHandle{
            _value,
            _inner
        };
    }

    template<class RT>
    NodeRef<T> ref(NodeHandle<RT>& referencer, bool isChild)
    {
        size_t id = _inner->_refIdCounter++;
        NodeRef<T> newRef(_value, id, isChild);
        if (isChild)
            _inner->_childRefs.push_back({id, referencer.downgrade()});
        else 
            _inner->_extRefs.push_back({id, referencer.downgrade()});
        return newRef;
    }

    std::shared_ptr<T> value()
    {
        return _value;
    }

    T& operator->() const 
    {
        return *_value;        
    }

    const NodeHandleRefList& externalRefs()
    {
        return _inner->_extRefs;
    }

    const NodeHandleRefList& childRefs()
    {
        return _inner->_childRefs;
    }
};


template<class T>
class NodeRef
{
    WeakNodeHandle _handle;
    size_t _refId;
    bool _isChild;

    NodeRef(WeakNodeHandle handle, size_t refId, bool isChild):
        _handle(std::move(handle)), _refId(refId), _isChild(isChild)
    {
    }

public:
    ~NodeRef()
    {
        if(auto handle = _handle.upgrade())
        {
            NodeHandleRefList& refList = _isChild ? handle->_inner->_childRefs : handle->_inner->_extRefs;

            auto begin = refList.begin();
            auto end = refList.end();

            for(auto iter = begin; iter != end; ++iter) 
            {
                if(iter->first == _refId)
                {
                    *iter = *end;
                    refList.erase(end);
                }
            }
        }
    }

    bool isChild()
    {
        return _isChild;
    }

    operator bool() const { return !_handle.expired(); }
    
    std::optional<NodeHandle<T>> upgrade()
    {
        if(auto handle = _handle.upgrade())
            return handle->as<T>();
        return std::nullopt;
    }
};

#endif
