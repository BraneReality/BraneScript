#ifndef BRANESCRIPT_REFERENCES_H
#define BRANESCRIPT_REFERENCES_H

#include <memory>
#include <vector>


template<typename T>
class Referenceable
{
    std::shared_ptr<T> _value;
    std::vector<std::weak_ptr<>>
};


#endif
