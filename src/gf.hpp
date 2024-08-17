#include <cstdint>
#include <cstddef>
#include <cstdarg>
#include <cassert>

#include "luigi.hpp"

// ---------------------------------------------------------------------------------------------
//                              Data structures
// ---------------------------------------------------------------------------------------------

inline uint64_t Hash(const uint8_t* key, size_t keyBytes) {
   uint64_t hash = 0xCBF29CE484222325;
   for (uintptr_t i = 0; i < keyBytes; i++)
      hash = (hash ^ key[i]) * 0x100000001B3;
   return hash;
}

template <class T>
struct Array {
   T*     array;
   size_t length, allocated;

   void InsertMany(T* newItems, uintptr_t index, size_t newCount) {
      if (length + newCount > allocated) {
         allocated *= 2;
         if (length + newCount > allocated)
            allocated = length + newCount;
         array = (T*)realloc(array, allocated * sizeof(T));
      }

      length += newCount;
      memmove(array + index + newCount, array + index, (length - index - newCount) * sizeof(T));
      memcpy(array + index, newItems, newCount * sizeof(T));
   }

   void Delete(uintptr_t index, size_t count = 1) {
      memmove(array + index, array + index + count, (length - index - count) * sizeof(T));
      length -= count;
   }

   bool Contains(T item, uintptr_t* index) {
      for (uintptr_t i = 0; i < length; i++) {
         if (array[i] == item) {
            if (index)
               *index = i;
            return true;
         }
      }

      return false;
   }

   void Insert(T item, uintptr_t index) { InsertMany(&item, index, 1); }
   void Add(T item) { Insert(item, length); }
   void Free() {
      free(array);
      array  = nullptr;
      length = allocated = 0;
   }
   int Length() { return length; }
   T&  First() { return array[0]; }
   T&  Last() { return array[length - 1]; }
   T&  operator[](uintptr_t index) {
      assert(index < length);
      return array[index];
   }
   void Pop() { length--; }
   void DeleteSwap(uintptr_t index) {
      if (index != length - 1)
         array[index] = Last();
      Pop();
   }
};

uint64_t Hash(const uint8_t* key, size_t keyBytes);

template <class K, class V>
struct MapShort {
   struct {
      K key;
      V value;
   }*     array;
   size_t used, capacity;

   V* At(K key, bool createIfNeeded) {
      if (used + 1 > capacity / 2) {
         MapShort grow        = {};
         grow.capacity        = capacity ? (capacity + 1) * 2 - 1 : 15;
         *(void**)&grow.array = calloc(sizeof(array[0]), grow.capacity);
         for (uintptr_t i = 0; i < capacity; i++)
            if (array[i].key)
               grow.Put(array[i].key, array[i].value);
         free(array);
         *this = grow;
      }

      uintptr_t slot = Hash((uint8_t*)&key, sizeof(key)) % capacity;
      while (array[slot].key && array[slot].key != key)
         slot = (slot + 1) % capacity;

      if (!array[slot].key && createIfNeeded) {
         used++;
         array[slot].key = key;
      }

      return &array[slot].value;
   }

   bool Has(K key) {
      if (!capacity)
         return false;
      uintptr_t slot = Hash((uint8_t*)&key, sizeof(key)) % capacity;
      while (array[slot].key && array[slot].key != key)
         slot = (slot + 1) % capacity;
      return array[slot].key;
   }

   V    Get(K key) { return *At(key, false); }
   void Put(K key, V value) { *At(key, true) = value; }
   void Free() {
      free(array);
      array = nullptr;
      used = capacity = 0;
   }
};

// ---------------------------------------------------------------------------------------------
//                              General
// ---------------------------------------------------------------------------------------------
struct InterfaceCommand {
   const char* label;
   UIShortcut  shortcut;
};

struct InterfaceWindow {
   const char* name;
   UIElement* (*create)(UIElement* parent);
   void (*update)(const char* data, UIElement* element);
   void (*focus)(UIElement* element);
   UIElement* element;
   bool       queuedUpdate, alwaysUpdate;
   void (*config)(const char* key, const char* value);
};

struct InterfaceDataViewer {
   const char* addButtonLabel;
   void (*addButtonCallback)(void* _unused);
};

struct INIState {
   char * buffer, *section, *key, *value;
   size_t bytes, sectionBytes, keyBytes, valueBytes;
};

struct ReceiveMessageType {
   UIMessage message;
   void (*callback)(char* input);
};
